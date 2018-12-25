{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Graphics.PDFKit.Helpers where

import Import
import Data.Time
import Text.Printf

formatLocalTime :: TimeZone -> UTCTime -> String
formatLocalTime timeZone utcTime = formatTime defaultTimeLocale "%Y%m%d%H%M%S" $ utcToLocalTime timeZone utcTime

maybeTextToText :: Maybe Text -> Text
maybeTextToText Nothing = ""
maybeTextToText (Just t) = t

maybeIntToText :: Maybe Int -> Text
maybeIntToText Nothing = ""
maybeIntToText (Just i) = pack . show $ i

intToText :: Int -> Text
intToText i = pack . show $ i

doubleToText :: Double -> Text
doubleToText x = pack $ printf "%.2f" x

ref :: Int -> Text
ref objId = intToText objId ++ " 0 R"

formatXrefPos :: Int -> Text
formatXrefPos i = pack $ printf "%010d" i

currentTimeZone :: Handler TimeZone
currentTimeZone = do
  timeZone <- liftIO $ getCurrentTimeZone
  return timeZone
