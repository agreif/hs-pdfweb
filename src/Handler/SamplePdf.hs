{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.SamplePdf where

import Import
import qualified Data.Text as T
import Handler.Graphics.PDFKit.Pdf
import Handler.Graphics.PDFKit.Types
import Handler.Graphics.PDFKit.Helpers
import Handler.Graphics.PDFKit.Encode

getPdfJsonR :: Handler Value
getPdfJsonR = do
  pdfDoc <- samplePdfDoc
  return $ toJSON
    ( pdfDoc
    , encodePdf' pdfDoc
    )

getPdfDocR :: Handler TypedContent
getPdfDocR = do
  pdfDoc <- samplePdfDoc
  addHeader "Content-Disposition" $
    T.concat ["attachment; filename=\"", "samplepdf.pdf", "\""]
  respond (encodeUtf8 "application/pdf") $ encodePdf pdfDoc

samplePdfDoc :: Handler PdfDocument
samplePdfDoc = do
  timeZone <- currentTimeZone
  now <- liftIO getCurrentTime
  let creationDate = pack $ formatLocalTime timeZone now
  return $ run creationDate $ do
    info
      >> infoProducer "ppppp"
      >> infoCreator "cccc"
    font
    resources
    page
    finalize
