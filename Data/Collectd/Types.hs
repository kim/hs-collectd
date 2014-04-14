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
