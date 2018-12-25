{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.SamplePdf where

import Import
import qualified Data.Text as T
import Handler.Graphics.PDFKit.PDFKit
import Handler.Graphics.PDFKit.Types
import Handler.Graphics.PDFKit.Helpers
import Handler.Graphics.PDFKit.Encode

getSamplePdfJsonR :: Handler Value
getSamplePdfJsonR = do
  pdfDoc <- samplePdfDoc
  return $ toJSON
    ( pdfDoc
    , encodePdf' pdfDoc
    )

getSamplePdfDocR :: Handler TypedContent
getSamplePdfDocR = do
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
    page
    finalize