{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Graphics.PDFKit.PDFKit where

import Import
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B8
import Handler.Graphics.PDFKit.Pdf

info :: PdfBuilder
info = build ActionInfoSetup

infoProducer :: Text -> PdfBuilder
infoProducer text = build $ ActionInfoSetProducer text

infoCreator :: Text -> PdfBuilder
infoCreator text = build $ ActionInfoSetCreator text

font :: PdfBuilder
font = build ActionFont

page :: PdfBuilder
page = do
  build ActionPage
  build ActionResources

pageSize :: PdfPageSize -> PdfBuilder
pageSize size = build $ ActionPageSetSize size

pageSizeCustom :: Double -> Double -> PdfBuilder
pageSizeCustom w h = build $ ActionPageSetSizeCustom w h

pageLayout :: PdfPageLayout -> PdfBuilder
pageLayout l = build $ ActionPageSetLayout l

pageMargin :: Double -> PdfBuilder
pageMargin x = build $ ActionPageSetMargin x

pageMargins :: Double -> Double -> Double -> Double -> PdfBuilder
pageMargins t l b r = build $ ActionPageSetMargins t l b r

-----------------------------------------------

run :: Text -> PdfBuilderM b -> PdfDocument
run creationDate (PdfBuilderM _ userActions) =
  L.foldl
  (\pdfDoc action -> execute action pdfDoc)
  (initialPdfDocument creationDate)
  (userActions ++ [ActionFinalize])

-----------------------------------------------

encodePdf :: PdfDocument -> ByteString
encodePdf pdfDoc = B8.unlines $ toByteStringLines pdfDoc pdfDoc

encodePdf' :: PdfDocument -> [Text]
encodePdf' pdfDoc = map decodeUtf8 $ toByteStringLines pdfDoc pdfDoc
