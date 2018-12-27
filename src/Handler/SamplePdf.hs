{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.SamplePdf where

import Import
import qualified Data.Text as T
import Data.Time
import Handler.Graphics.PDFKit.PDFKit
import Handler.Graphics.PDFKit.Pdf

getSamplePdfJsonR :: Handler Value
getSamplePdfJsonR = do
  pdfDoc <- samplePdfDoc
  return $
    toJSON
    ( pdfDoc
    , encodePdf' pdfDoc
    )

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

samplePdfDoc :: Handler PdfDocument
samplePdfDoc = do
  timeZone <- liftIO getCurrentTimeZone
  now <- liftIO getCurrentTime
  return $ buildPdfDoc now timeZone $ do
    producer "sample producer"
    creator "sample creator"
    page
      >> pageSize sA4
      >> pageLayout landscape
    font courier
    text "abc" 99 199
    font courierBold
    text "def" 99 99
