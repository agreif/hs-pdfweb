{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.SamplePdf where

import Import
import qualified Data.Text as T
import Data.Time
import Handler.Graphics.PDFKit.PDFKit
import Handler.Graphics.PDFKit.Pdf

samplePdfDoc :: Handler PdfDocument
samplePdfDoc = do
  timeZone <- liftIO getCurrentTimeZone
  now <- liftIO getCurrentTime
  return $ buildPdfDoc now timeZone $ do
    producer "sample producer"
    creator "sample creator"
    page
      >> pageSize sLetter
      >> pageLayout portrait
    font courierBold
    textAt "x:100 y:100" 100 100
    fontSize 12
    font courier
    textAt "x:100 y:200" 100 200

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
