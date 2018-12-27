{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Graphics.PDFKit.PDFKit where

import Import
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B8
import Data.Time
import Handler.Graphics.PDFKit.Pdf

producer :: Text -> PdfBuilder
producer = build . ActionInfoSetProducer

creator :: Text -> PdfBuilder
creator = build . ActionInfoSetCreator

font :: PdfStandardFont -> PdfBuilder
font = build . ActionFont

page :: PdfBuilder
page = build ActionPage

pageSize :: PdfPageSize -> PdfBuilder
pageSize = build . ActionPageSetSize

pageSizeCustom :: Double -> Double -> PdfBuilder
pageSizeCustom w h = build $ ActionPageSetSizeCustom w h

pageLayout :: PdfPageLayout -> PdfBuilder
pageLayout = build . ActionPageSetLayout

pageMargin :: Double -> PdfBuilder
pageMargin = build . ActionPageSetMargin

pageMargins :: Double -> Double -> Double -> Double -> PdfBuilder
pageMargins t l b r = build $ ActionPageSetMargins t l b r

text :: Text -> Double -> Double -> PdfBuilder
text t x y = do
  build ActionFontAddIfMissing
  build $ ActionText t x y

-----------------------------------------------

buildPdfDoc :: UTCTime -> TimeZone -> PdfBuilderM b -> PdfDocument
buildPdfDoc now timeZone (PdfBuilderM _ userActions) =
  L.foldl
  (\pdfDoc action -> execute action pdfDoc)
  (initialPdfDocument now timeZone)
  (userActions ++ [ActionFinalize])

-----------------------------------------------

encodePdf :: PdfDocument -> ByteString
encodePdf pdfDoc = B8.unlines $ toByteStringLines pdfDoc

encodePdf' :: PdfDocument -> [Text]
encodePdf' pdfDoc = map decodeUtf8 $ toByteStringLines pdfDoc
