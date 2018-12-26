{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.SamplePdf where

import Import
import qualified Data.Text as T
import Handler.Graphics.PDFKit.PDFKit
import Handler.Graphics.PDFKit.Pdf
import Handler.Graphics.PDFKit.Helpers
import Handler.Graphics.PDFKit.Encode

getSamplePdfJsonR :: Handler Value
getSamplePdfJsonR = do
  pdfDoc <- samplePdfDoc
  return $ toJSON
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
  timeZone <- currentTimeZone
  now <- liftIO getCurrentTime
  let creationDate = pack $ formatLocalTime timeZone now
  return $ run creationDate $ do
    info
      >> infoProducer "the producer"
      >> infoCreator "the creator"
    font
    page
      >> pageSize sizeLETTER
      >> pageSizeCustom 100 200.99
      >> pageLayout landscape
      >> pageMargin 123
      >> pageMargins 123 124 125 126
