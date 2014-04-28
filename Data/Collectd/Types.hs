module Data.Collectd.Types
where

import Data.Text             (Text)
import Data.Time.Clock.POSIX
import Data.Word


type Host       = Text
type Plugin     = Text
type PluginInst = Text
type Typ        = Text
type TypInst    = Text
type Message    = Text


-- host "/" plugin ["-" plugin instance] "/" type ["-" type instance]
data Identifier = Identifier
    { idHost       :: !Host
    , idPlugin     :: !Plugin
    , idPluginInst :: Maybe PluginInst
    , idTyp        :: !Typ
    , idTypInst    :: Maybe TypInst
    } deriving (Eq, Show)

data NotificationIdentifier = NotificationIdentifier
    { nidHost       :: Maybe Host
    , nidPlugin     :: Maybe Plugin
    , nidPluginInst :: Maybe PluginInst
    , nidTyp        :: Maybe Typ
    , nidTypInst    :: Maybe TypInst
    } deriving (Eq, Show)

data Value
    = Counter  !Word64
    | Absolute !Word64
    | Derive   !Int
    | Gauge    !Double
    deriving (Eq, Show)

newtype Timestamp = Timestamp { posix :: POSIXTime }
    deriving (Eq, Show)

data Time
    = Stamp !Timestamp
    | Now
    deriving (Eq, Show)

data ValueList = ValueList !Time [Value]
    deriving (Eq, Show)

data Option = Interval Word64
    deriving (Eq, Show)

data Request
    = GetVal   !Identifier
    | ListVal
    | PutVal   !Identifier [Option] !ValueList
    | PutNotif !Message !Severity !Timestamp !NotificationIdentifier
    | Flush    (Maybe Int) [Plugin] [Identifier]
    deriving (Eq, Show)

data Notification = Notification [Header] !Message
    deriving (Eq, Show)

data Header
    = Severity       !Severity
    | Time           !Double
    | Host           !Text
    | Plugin         !Text
    | PluginInstance !Text
    | Type           !Text
    | TypeInstance   !Text
    | UnknownHeader  !Text !Text
    deriving (Eq, Show)

data Severity
    = Failure
    | Warning
    | Okay
    deriving (Eq, Show)


notifSeverity :: Notification -> Severity
notifSeverity (Notification hdrs _) = head [ s | Severity s <- hdrs ]

notifTime :: Notification -> Double
notifTime (Notification hdrs _) = head [ t | Time t <- hdrs ]

notifMessage :: Notification -> Message
notifMessage (Notification _ m) = m

notifHost :: Notification -> Maybe Host
notifHost (Notification hdrs _) = case [ h | Host h <- hdrs ] of
    []    -> Nothing
    (h:_) -> Just h

notifPlugin :: Notification -> Maybe Plugin
notifPlugin (Notification hdrs _) = case [ p | Plugin p <- hdrs ] of
    []    -> Nothing
    (p:_) -> Just p

notifPluginInst :: Notification -> Maybe PluginInst
notifPluginInst (Notification hdrs _) = case [ p | PluginInstance p <- hdrs ] of
    []    -> Nothing
    (p:_) -> Just p

notifTyp :: Notification -> Maybe Typ
notifTyp (Notification hdrs _) = case [ t | Type t <- hdrs ] of
    []    -> Nothing
    (t:_) -> Just t

notifTypInst :: Notification -> Maybe TypInst
notifTypInst (Notification hdrs _) = case [ t | TypeInstance t <- hdrs ] of
    []    -> Nothing
    (t:_) -> Just t

notifOtherHeaders :: Notification -> [(Text,Text)]
notifOtherHeaders (Notification hdrs _) = [ (k,v) | UnknownHeader k v <- hdrs ]

notifLookupHeader :: Notification -> Text -> Maybe Text
notifLookupHeader notif name = lookup name (notifOtherHeaders notif)
