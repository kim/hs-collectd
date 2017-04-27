module Data.Collectd.Binary
    ( Part (..)
    )
where

import Control.Applicative
import Control.Monad       (forM, forM_, replicateM)
import Data.ByteString     (ByteString)
import Data.Collectd.Types
import Data.Serialize
import Data.Text           (Text)
import Data.Text.Encoding
import Data.Word

import qualified Data.ByteString as B

import Prelude -- silence post-AMP complaints about importing Control.Applicative


data Part
    = PHost          !Host
    | PTime          !Timestamp
    | PTimeHiRes     !Word64
    | PPlugin        !Plugin
    | PPluginInst    !PluginInst
    | PTyp           !Typ
    | PTypInst       !TypInst
    | PValues        [Value]
    | PInterval      !Word64
    | PIntervalHiRes !Word64
    | PMessage       !Message
    | PSeverity      !Severity
    | PSignature     !ByteString
    | PEncryption    !ByteString
    deriving (Eq, Show)

instance Serialize Part where
    put = putPart
    get = getPart


putPart :: Putter Part
putPart part = case part of
    PHost h -> putWord16be 0x0000 >> putText h

    PTime t       -> putWord16be 0x0001 >> putWord16be 12 >> putTimestamp t
    PTimeHiRes t  -> putWord16be 0x0008 >> putWord16be 12 >> put t

    PPlugin p     -> putWord16be 0x0002 >> putText p
    PPluginInst p -> putWord16be 0x0003 >> putText p
    PTyp t        -> putWord16be 0x0004 >> putText t
    PTypInst t    -> putWord16be 0x0005 >> putText t

    PValues vs -> do
        putWord16be 0x0006
        putWord16be $ fromIntegral (length vs) + 4
        forM_ vs $ \ v -> putWord8 $ case v of
            Counter  _ -> 0
            Gauge    _ -> 1
            Derive   _ -> 2
            Absolute _ -> 3
        forM_ vs $ \ v -> case v of
            Counter  i -> putWord64be  i
            Gauge    i -> putFloat64le i
            Derive   i -> put i
            Absolute i -> putWord64be  i

    PInterval i      -> putWord16be 0x0007 >> putWord16be 12 >> put i
    PIntervalHiRes i -> putWord16be 0x0009 >> putWord16be 12 >> put i

    PMessage m -> putWord16be 0x0100 >> putText m

    PSeverity s -> do
        putWord16be 0x0101
        putWord16be 6
        putWord16be $ case s of
            Failure -> 1
            Warning -> 2
            Okay    -> 4

    PSignature  bs -> putWord16be 0x0200 >> putBytes bs
    PEncryption bs -> putWord16be 0x0210 >> putBytes bs
  where
    putText :: Putter Text
    putText t = putBytes $ encodeUtf8 t

    putTimestamp :: Putter Timestamp
    putTimestamp = putWord16be . round . posix

    putBytes :: Putter ByteString
    putBytes bs = do
        putWord16be $ fromIntegral (B.length bs) + 4 + 1
        put bs
        putWord8 0

getPart :: Get Part
getPart = do
    ptyp <- getWord16be
    plen <- getWord16be
    case ptyp of
        0x0000 -> PHost <$> getText plen

        0x0001 -> PTime . Timestamp . fromIntegral <$> getWord64be
        0x0008 -> PTimeHiRes <$> getWord64be

        0x0002 -> PPlugin     <$> getText plen
        0x0003 -> PPluginInst <$> getText plen
        0x0004 -> PTyp        <$> getText plen
        0x0005 -> PTypInst    <$> getText plen

        0x0006 -> do
            nvals <- getWord16be
            typs  <- replicateM (fromIntegral nvals) getWord8
            vals  <- forM typs $ \ typ -> case typ of
                0 -> Counter  <$> getWord64be
                1 -> Gauge    <$> getFloat64le
                2 -> Derive   <$> get
                3 -> Absolute <$> getWord64be
                x -> fail $ "Unknown value type: " ++ show x
            return $ PValues vals

        0x0007 -> PInterval      <$> getWord64be
        0x0009 -> PIntervalHiRes <$> getWord64be

        0x0100 -> PMessage <$> getText plen

        0x0101 -> do
            sev <- getWord16be
            case sev of
                1 -> return $ PSeverity Failure
                2 -> return $ PSeverity Warning
                4 -> return $ PSeverity Okay
                x -> fail $ "Unknown severity: " ++ show x

        0x0200 -> PSignature  <$> getByteString (fromIntegral plen - 4)
        0x0210 -> PEncryption <$> getByteString (fromIntegral plen - 4)

        _ -> fail $ "Unknown part type: " ++ show ptyp
  where
    getText :: Word16 -> Get Text
    getText len = decodeUtf8 <$> B.init <$> getByteString (fromIntegral len - 4)
