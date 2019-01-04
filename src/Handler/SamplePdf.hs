{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.SamplePdf where

import qualified Data.Text as T
import Data.Time
import Import
import PdfKit

samplePdfDoc :: Handler PdfDocument
samplePdfDoc = do
  timeZone <- liftIO getCurrentTimeZone
  now <- liftIO getCurrentTime
  return $
    buildPdfDoc $ do
      producer "sample producer"
      creator "sample creator"
      creationDate now timeZone
      pageA4Landscape $ do
        text $ do
          content "ü ä=ã, ö=õ, ü=ũ"
          font courier
          fontSize 12
        textHelvetica24 $ do
          textPos 100 100
          content "ü ä=ã, ö=õ, ü=ũ"
        textHelvetica24 $ do
          content "foo"
        textHelvetica24 $ do
          content "bar"
  where
    pageA4Landscape :: PdfPageBuilder -> PdfDocumentBuilder
    pageA4Landscape = pageTemplate $ do
      pageSize sLetter
      layout landscape
    textHelvetica24 :: PdfTextBuilder -> PdfPageBuilder
    textHelvetica24 = textTemplate $ do
      font helvetica
      fontSize 24

getSamplePdfJsonR :: Handler Value
getSamplePdfJsonR = do
  pdfDoc <- samplePdfDoc
  let pdfDoc' = pdfDoc {pdfDocumentHeaderLines = []}
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
