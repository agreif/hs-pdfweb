# hs-pdfweb
Sample Yesod App to generate PDF with a self-written PDF-renderer

## The PDF-renderer

see 'src/Handler/Pdf.hs'

## Sample

```haskell
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
```
