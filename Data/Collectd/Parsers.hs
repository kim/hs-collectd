{-# LANGUAGE OverloadedStrings #-}

module Data.Collectd.Parsers
    ( notification
    )
where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Collectd.Types
import Data.Text            (strip)


notification :: Parser Notification
notification = Notification <$> headers <*> body
  where
    headers = manyTill (header <* endOfLine) (try endOfLine)
    body    = strip <$> takeText

header :: Parser Header
header = do
    k <- takeTill (== ':') <* char ':' <* skipSpace
    case k of
        "Severity" -> Severity <$> severity
        "Time"     -> Time     <$> double
        "Host"     -> Host     <$> tillEol
        "Plugin"   -> Plugin   <$> tillEol
        "Type"     -> Type     <$> tillEol
        "PluginInstance" -> PluginInstance <$> tillEol
        "TypeInstance"   -> TypeInstance   <$> tillEol
        _  -> UnknownHeader k <$> tillEol
  where
    tillEol = takeTill isEndOfLine

severity :: Parser Severity
severity =  (const Failure <$> string "FAILURE")
        <|> (const Warning <$> string "WARNING")
        <|> (const Okay    <$> string "OKAY")
