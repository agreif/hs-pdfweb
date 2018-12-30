{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Graphics.PDFKit.PDFKit where

import Import
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B8
import Data.Time
import Handler.Graphics.PDFKit.Pdf

producer :: Text -> DocumentBuilder
producer = documentAction . ActionInfoSetProducer

creator :: Text -> DocumentBuilder
creator = documentAction . ActionInfoSetCreator

page :: PageBuilderM a -> DocumentBuilder
page (PageBuilderM userActions _) = documentAction $ ActionComposite actions
  where
    actions = [ActionPage] ++ userActions

font :: PdfStandardFont -> PageBuilder
font = pageAction . ActionFont

fontSize :: Int -> PageBuilder
fontSize = pageAction . ActionFontSetSize

pageSize :: PdfPageSize -> PageBuilder
pageSize = pageAction . ActionPageSetSize

pageSizeCustom :: Double -> Double -> PageBuilder
pageSizeCustom w h = pageAction $ ActionPageSetSizeCustom w h

layout :: PdfPageLayout -> PageBuilder
layout = pageAction . ActionPageSetLayout

margin :: Double -> PageBuilder
margin = pageAction . ActionPageSetMargin

margins :: Double -> Double -> Double -> Double -> PageBuilder
margins t l b r = pageAction $ ActionPageSetMargins t l b r

textAt :: Text -> Double -> Double -> PageBuilder
textAt t x y = do
  pageAction ActionMoveDown
  pageAction ActionFontAddIfMissing
  pageAction $ ActionTextAt t x y

text :: Text -> PageBuilder
text t = do
  pageAction ActionMoveDown
  pageAction ActionFontAddIfMissing
  pageAction $ ActionText t

moveDown :: PageBuilder
moveDown = pageAction ActionMoveDown

-----------------------------------------------

buildPdfDoc :: UTCTime -> TimeZone -> DocumentBuilderM a -> PdfDocument
buildPdfDoc now timeZone (DocumentBuilderM userActions _) =
  L.foldl
  (\pdfDoc action -> execute action pdfDoc)
  (initialPdfDocument now timeZone)
  (userActions ++ [ActionFinalize])

-----------------------------------------------

encodePdf :: PdfDocument -> ByteString
encodePdf pdfDoc = B8.unlines $ toByteStringLines pdfDoc

encodePdf' :: PdfDocument -> [Text]
encodePdf' pdfDoc = map decodeUtf8 $ toByteStringLines pdfDoc
