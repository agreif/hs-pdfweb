{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Graphics.PDFKit.Encode where

import Import
import qualified Data.ByteString.Char8 as B8
import Handler.Graphics.PDFKit.Pdf


encodePdf :: PdfDocument -> ByteString
encodePdf pdfDoc = B8.unlines $ toByteStringLines pdfDoc pdfDoc

encodePdf' :: PdfDocument -> [Text]
encodePdf' pdfDoc = map decodeUtf8 $ toByteStringLines pdfDoc pdfDoc
