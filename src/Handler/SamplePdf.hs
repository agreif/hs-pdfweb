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
      >> pageSize sA4
      >> pageLayout portrait
    font courierBold
    text "default x y"
    text "foo ATyg"
    text "bar ATyg"
    text "baz ATyg"
    font timesRoman
    textAt "x:300 y:100" 300 100
    text "foo2 ATyg"
    text "bar2 ATyg"
    fontSize 12
    font helveticaOblique
    textAt "x:300 y:200" 300 200
    text "foo3 ATyg"
    text "bar3 ATyg"

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
