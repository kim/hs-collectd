{-# LANGUAGE OverloadedStrings #-}

module Data.Collectd.Plugin
    ( Exec
    , getEnv
    , getPrev
    , putVal
    , putNotif
    , putNotifSimple
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
import Data.Time.Clock.POSIX   (getPOSIXTime)
import Network.BSD             (getHostName)
import Prelude                 hiding (putStrLn)
import System.Environment      (lookupEnv)
import System.IO               (hFlush, stdout)
import Text.Read               (readMaybe)


newtype Ident = Ident { runIdent :: Typ -> Maybe TypInst -> Identifier }

newtype NotifIdent = NotifIdent
    { runNotifIdent :: Maybe Typ
                    -> Maybe TypInst
                    -> NotificationIdentifier }

data Env a b = Env
    { _env   :: !a
    , _prev  :: Maybe b
    , _ident :: Ident
    , _nid   :: NotifIdent
    , _opts  :: [Option]
    }

type Exec a b c = ReaderT (Env a b) IO c

getEnv :: Exec a b a
getEnv = asks _env

getPrev :: Exec a b (Maybe b)
getPrev = asks _prev

schedExec :: Plugin -> Maybe PluginInst -> a -> Exec a b b -> IO ()
schedExec p mpInst a m = do
    h <- host =<< lookupEnv "COLLECTD_HOST"
    i <- intv =<< lookupEnv "COLLECTD_INTERVAL"

    let idt = mkIdent (pack h) p mpInst
        nid = mkNid   (pack h) p mpInst
        opt = maybe [] ((:[]) . Interval) i
        env = Env a Nothing idt nid opt
        run = runReaderT m
     in loop run env i
  where
    loop run env i = do
        new <- run env
        hFlush stdout
        case i of
            Just sec | sec > 0 -> do
                threadDelay (1000 * 1000 * fromIntegral sec)
                loop run env { _prev = Just new } i
            _ -> return ()

    host = maybe getHostName pure
    intv = pure . maybe Nothing readMaybe

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

putNotif :: Maybe Typ
         -> Maybe TypInst
         -> Message
         -> Severity
         -> Timestamp
         -> Exec a b ()
putNotif mtyp mtypInst msg sev t = do
    nid <- getNid
    liftIO . dispatch $
        PutNotif msg sev t (runNotifIdent nid mtyp mtypInst)

putNotifSimple :: Message -> Severity -> Exec a b ()
putNotifSimple msg sev = do
    now <- Timestamp <$> liftIO getPOSIXTime
    putNotif Nothing Nothing msg sev now


--------------------------------------------------------------------------------

getIdent :: Exec a b Ident
getIdent = asks _ident

getNid :: Exec a b NotifIdent
getNid = asks _nid

getOpts :: Exec a b [Option]
getOpts = asks _opts

mkIdent :: Host -> Plugin -> Maybe PluginInst -> Ident
mkIdent h p mpi = Ident (Identifier h p mpi)

mkNid :: Host -> Plugin -> Maybe PluginInst -> NotifIdent
mkNid h p mpi = NotifIdent (NotificationIdentifier (Just h) (Just p) mpi)

dispatch :: Request -> IO ()
dispatch = putStrLn . toLazyText . formatRequest
