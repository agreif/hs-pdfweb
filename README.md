# hs-pdfweb
Sample Yesod App to generate PDF with a self-written PDF-renderer

## The PDF-renderer

see 'src/Handler/Pdf.hs'

## Sample

```haskell
samplePdfDoc :: Handler PdfDocument
samplePdfDoc = do
  timeZone <- liftIO getCurrentTimeZone
  now <- liftIO getCurrentTime
  return $ buildPdfDoc now timeZone $ do
    producer "sample producer"
    creator "sample creator"
    page $ do
      pageSize sA4
      layout landscape
      text "default x y"
      text "foo ATyg"
      font courierBold
      text "bar ATyg"
      text "baz ATyg"
      textAt "x:300 y:100" 300 100
      font timesRoman
      text "foo2 ATyg"
      text "bar2 ATyg"
      textAt "x:300 y:200" 300 200
      fontSize 12
      font helveticaOblique
      text "foo3 ATyg"
      text "bar3 ATyg"
    page $ do
      pageSize sA5
      layout portrait
      text "default x y"
      text "foo ATyg"

-- Yesod Handler
getSamplePdfInlineR :: Handler TypedContent
getSamplePdfInlineR = do
  pdfDoc <- samplePdfDoc
  addHeader "Content-Disposition" $
    T.concat ["inline; filename=\"", "samplepdf.pdf", "\""]
  respond (encodeUtf8 "application/pdf") $ encodePdf pdfDoc

-- Yesod Handler
getSamplePdfDownloadR :: Handler TypedContent
getSamplePdfDownloadR = do
  pdfDoc <- samplePdfDoc
  addHeader "Content-Disposition" $
    T.concat ["attachment; filename=\"", "samplepdf.pdf", "\""]
  respond (encodeUtf8 "application/pdf") $ encodePdf pdfDoc

-- Yesod Handler
getSamplePdfJsonR :: Handler Value
getSamplePdfJsonR = do
  pdfDoc <- samplePdfDoc
  return $
    toJSON
    ( pdfDoc
    , encodePdf' pdfDoc
    )
```
