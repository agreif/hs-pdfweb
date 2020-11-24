{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.SamplePdf where

import qualified Data.Text as T
import Data.Time
import Import
import PdfKit

samplePdfDoc :: Handler PdfDocument
samplePdfDoc = do
  timeZone <- liftIO getCurrentTimeZone
  now <- liftIO getCurrentTime
  return
    $ buildPdfDoc
    $ do
      producer "sample producer"
      creator "sample creator"
      creationDate now timeZone
      pageA4Landscape $ do
        line $ do
          linePoint 100 100
          linePoint 100 200
          linePoint 200 200
          lineWidth 10
          lineStroke
        text $ do
          textPos 180 190
          textColorCmyk 100 100 0 0
          textFillOpacity 1
          font helvetica
          fontSize 40
          content "foo bar"
        textHelvetica24BlueOpaque $ do
          content "ü ä=ã, ö=õ, ü=ũ"
  where
    pageA4Landscape :: PdfPageBuilder -> PdfDocumentBuilder
    pageA4Landscape =
      pageTemplate $ do
        pageSize sLetter
        layout landscape
    textHelvetica24BlueOpaque :: PdfTextBuilder -> PdfPageBuilder
    textHelvetica24BlueOpaque =
      textTemplate $ do
        font helvetica
        fontSize 24
        textColorRgb 0 0 255
        textFillOpacity 0.5

getSamplePdfJsonR :: Handler Value
getSamplePdfJsonR = do
  pdfDoc <- samplePdfDoc
  let pdfDoc' = pdfDoc {_pdfDocumentHeaderLines = []}
  return $ toJSON (pdfDoc', encodePdf' pdfDoc')

getSamplePdfInlineR :: Handler TypedContent
getSamplePdfInlineR = do
  pdfDoc <- samplePdfDoc
  addHeader "Content-Disposition" $
    T.concat ["inline; filename=\"", "samplepdf.pdf", "\""]
  respond (encodeUtf8 "application/pdf") $ encodePdf pdfDoc

getSamplePdfDownloadR :: Handler TypedContent
getSamplePdfDownloadR = do
  pdfDoc <- samplePdfDoc
  addHeader "Content-Disposition" $
    T.concat ["attachment; filename=\"", "samplepdf.pdf", "\""]
  respond (encodeUtf8 "application/pdf") $ encodePdf pdfDoc
