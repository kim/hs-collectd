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
    headers = header `sepBy` "\n" <* char '\n' <* skipSpace
    body    = strip <$> takeTill (== '\n') <* skipSpace

header :: Parser Header
header = do
    k <- takeTill (== ':') <* char ':' <* skipSpace
    case k of
        "Severity" -> Severity <$> severity
        "Time"     -> Time     <$> double
        "Host"     -> Host     <$> takeTill (== '\n')
        "Plugin"   -> Plugin   <$> takeTill (== '\n')
        "Type"     -> Type     <$> takeTill (== '\n')
        "PluginInstance" -> PluginInstance <$> takeTill (== '\n')
        "TypeInstance"   -> TypeInstance   <$> takeTill (== '\n')
        _  -> UnknownHeader k <$> takeTill (== '\n')

severity :: Parser Severity
severity =  (const Failure <$> string "FAILURE")
        <|> (const Warning <$> string "WARNING")
        <|> (const Okay    <$> string "OKAY")
