{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Data.Collectd.Plugin
    ( Exec
    , getEnv
    , getPrev
    , putVal
    , schedExec
    , module PT
    )
where

import Control.Applicative
import Control.Concurrent      (threadDelay)
import Control.Monad.Reader
import Data.Collectd.PlainText as PT
import Data.Maybe
import Data.Text               (pack)
import Data.Text.Lazy.Builder  (toLazyText)
import Data.Text.Lazy.IO       (putStrLn)
import Network.BSD             (getHostName)
import Prelude                 hiding (putStrLn)
import System.Environment      (lookupEnv)
import System.IO               (hFlush, stdout)


newtype Ident = Ident { runIdent :: Typ -> Maybe TypInst -> Identifier }

data Env a b = Env
    { _env   :: !a
    , _prev  :: Maybe b
    , _ident :: Ident
    , _opts  :: [Option]
    }

type Exec a b c = ReaderT (Env a b) IO c

getEnv :: Exec a b a
getEnv = asks _env

getPrev :: Exec a b (Maybe b)
getPrev = asks _prev

schedExec :: Plugin -> Maybe PluginInst -> a -> Exec a b b -> IO ()
schedExec p mpInst a m = do
    h <- maybe getHostName return =<< lookupEnv "COLLECTD_HOST"
    i <- fmap read <$> lookupEnv "COLLECTD_INTERVAL"

    let idt = mkIdent (pack h) p mpInst
        opt = maybe [] ((:[]) . Interval) i
        env = Env a Nothing idt opt
        run = runReaderT m
     in loop run env i
  where
    loop run env i = do
        new <- run env
        hFlush stdout
        case i of
            Nothing  -> return ()
            Just sec -> threadDelay (1000 * 1000 * fromIntegral sec)
                     >> loop run env { _prev = Just new } i

putVal :: Maybe Typ -> Maybe TypInst -> Value -> Exec a b ()
putVal mtyp mtypInst val = do
    opts  <- getOpts
    ident <- getIdent

    liftIO . dispatch $
        PutVal (runIdent ident typ mtypInst) opts (ValueList Now [val])
  where
    typ  = fromMaybe vtyp mtyp
    vtyp = case val of
        Absolute _ -> "absolute"
        Counter  _ -> "counter"
        Derive   _ -> "derive"
        Gauge    _ -> "gauge"


--------------------------------------------------------------------------------

getIdent :: Exec a b Ident
getIdent = asks _ident

getOpts :: Exec a b [Option]
getOpts = asks _opts

mkIdent :: Host -> Plugin -> Maybe PluginInst -> Ident
mkIdent h p mpi = Ident (Identifier h p mpi)

dispatch :: Request -> IO ()
dispatch = putStrLn . toLazyText . formatRequest
