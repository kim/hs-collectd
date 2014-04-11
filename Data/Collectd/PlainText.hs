{-# LANGUAGE OverloadedStrings #-}

module Data.Collectd.PlainText
    ( formatRequest
    , parseNotification
    , module Types
    )
where

import Data.Attoparsec.Text
import Data.Collectd.Parsers
import Data.Collectd.Types   as Types
import Data.List
import Data.Monoid
import Data.Text             (Text)

import qualified Data.Text.Lazy.Builder           as L
import qualified Data.Text.Lazy.Builder.Int       as L
import qualified Data.Text.Lazy.Builder.RealFloat as L


parseNotification :: Text -> Either String Notification
parseNotification = parseOnly notification

formatRequest :: Request -> L.Builder
formatRequest rq = case rq of
    GetVal ident -> lst ["GETVAL", bIdentifier ident]
    ListVal      -> L.fromText "LISTVAL"

    PutVal ident opts vals ->
        lst [ lst $ "PUTVAL" : bIdentifier ident : bOptions opts
            , bValues vals
            ]

    PutNotif msg sev t nident ->
        lst [ "PUTNOTIF"
            , L.fromText ("message=" <> msg)
            , bSeverity sev
            , "time=" <> bTimestamp t
            , bNotificationIdentifier nident
            ]

    Flush timeout plugins idents ->
        lst [ "FLUSH"
            , maybe mempty (mappend "timeout=" . L.decimal) timeout
            , lst $ map (mappend "plugin="     . L.fromText ) plugins
            , lst $ map (mappend "identifier=" . bIdentifier) idents
            ]

--------------------------------------------------------------------------------

lst :: [L.Builder] -> L.Builder
lst = mconcat . intersperse " "

bIdentifier :: Identifier -> L.Builder
bIdentifier (Identifier host plugin pluginInst typ typInst) =
       L.fromText host
    <> "/"
    <> L.fromText plugin
    <> maybe "" (("-" <>) . L.fromText) pluginInst
    <> "/"
    <> L.fromText typ
    <> maybe "" (("-" <>) . L.fromText) typInst

bNotificationIdentifier :: NotificationIdentifier -> L.Builder
bNotificationIdentifier (NotificationIdentifier mhost mplugin mpluginInst mTyp mTypInst) =
    lst [ maybe mempty (mappend "host="            . L.fromText) mhost
        , maybe mempty (mappend "plugin="          . L.fromText) mplugin
        , maybe mempty (mappend "plugin_instance=" . L.fromText) mpluginInst
        , maybe mempty (mappend "type="            . L.fromText) mTyp
        , maybe mempty (mappend "type_instance="   . L.fromText) mTypInst
        ]

bOptions :: [Option] -> [L.Builder]
bOptions = map fromOption
  where
    fromOption (Interval i) = "interval=" <> L.decimal i

bValues :: ValueList -> L.Builder
bValues (ValueList t vs) = mconcat . intersperse ":" $ bTime t : map bValue vs

bTime :: Time -> L.Builder
bTime (Stamp s) = bTimestamp s
bTime Now       = "N"

bValue :: Value -> L.Builder
bValue (Counter  a) = L.decimal a
bValue (Absolute a) = L.decimal a
bValue (Derive   a) = L.decimal a
bValue (Gauge    a) = L.realFloat a

bTimestamp :: Timestamp -> L.Builder
bTimestamp = L.decimal . (round . posix :: Timestamp -> Int)

bSeverity :: Severity -> L.Builder
bSeverity s = L.fromText "severity=" <> case s of
    Failure -> "failure"
    Warning -> "warning"
    Okay    -> "okay"
