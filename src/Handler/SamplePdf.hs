{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.SamplePdf where

import qualified Data.Text as T
import Data.Time
import Import
import PdfKit
import PdfKit.Helper

samplePdfDoc :: Handler PdfDocument
samplePdfDoc = do
  timeZone <- liftIO getCurrentTimeZone
  maybeHelvetica <- liftIO mkAfmStdFontHelvetica
  maybeCourier <- liftIO mkAfmStdFontCourier
  maybeCourierBold <- liftIO mkAfmStdFontCourierBold
  maybeCourierOblique <- liftIO mkAfmStdFontCourierOblique
  maybeCourierBoldOblique <- liftIO mkAfmStdFontCourierBoldOblique
  maybeHelvetica <- liftIO mkAfmStdFontHelvetica
  maybeHelveticaBold <- liftIO mkAfmStdFontHelveticaBold
  maybeHelveticaOblique <- liftIO mkAfmStdFontHelveticaOblique
  maybeHelveticaBoldOblique <- liftIO mkAfmStdFontHelveticaBoldOblique
  maybeTimesRoman <- liftIO mkAfmStdFontTimesRoman
  maybeTimesBold <- liftIO mkAfmStdFontTimesBold
  maybeTimesItalic <- liftIO mkAfmStdFontTimesItalic
  maybeTimesBoldItalic <- liftIO mkAfmStdFontTimesBoldItalic
  maybeSymbol <- liftIO mkAfmStdFontSymbol
  liftIO $ print maybeSymbol
  maybeZapfDingbats <- liftIO mkAfmStdFontZapfDingbats
  now <- liftIO getCurrentTime
  let text' = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Mauris nunc congue nisi vitae. Amet purus gravida quis blandit turpis cursus. Et odio pellentesque diam volutpat commodo sed egestas egestas. Accumsan in nisl nisi scelerisque eu ultrices vitae. Malesuada fames ac turpis egestas integer eget aliquet nibh praesent. Proin fermentum leo vel orci porta. Ornare aenean euismod elementum nisi quis. Dolor morbi non arcu risus quis. Sit amet consectetur adipiscing elit duis tristique sollicitudin. In cursus turpis massa tincidunt dui ut ornare lectus. Quisque egestas diam in arcu cursus euismod.\nEt sollicitudin ac orci phasellus egestas tellus. Pharetra sit amet aliquam id diam. Aliquam sem et tortor consequat id porta nibh venenatis. Pharetra pharetra massa massa ultricies. Tristique risus nec feugiat in fermentum. Facilisis sed odio morbi quis commodo. Gravida rutrum quisque non tellus orci. Sed ullamcorper morbi tincidunt ornare massa. Pellentesque elit ullamcorper dignissim cras tincidunt. Adipiscing tristique risus nec feugiat in fermentum posuere. Morbi non arcu risus quis varius quam quisque id diam. Augue eget arcu dictum varius duis at consectetur. Sed vulputate odio ut enim blandit volutpat maecenas volutpat. Tempus quam pellentesque nec nam. Ut porttitor leo a diam sollicitudin tempor. Sagittis nisl rhoncus mattis rhoncus urna neque viverra justo nec. Id aliquet lectus proin nibh nisl condimentum id. Cras tincidunt lobortis feugiat vivamus. Blandit turpis cursus in hac habitasse platea dictumst quisque sagittis. Pellentesque habitant morbi tristique senectus et netus et malesuada fames.\nViverra accumsan in nisl nisi scelerisque eu ultrices vitae. In metus vulputate eu scelerisque. Faucibus turpis in eu mi bibendum neque egestas congue quisque. Sed tempus urna et pharetra pharetra. Sed id semper risus in hendrerit gravida rutrum. Cursus in hac habitasse platea dictumst quisque sagittis purus sit. A erat nam at lectus urna duis convallis. Ac ut consequat semper viverra nam. Viverra tellus in hac habitasse platea dictumst vestibulum rhoncus est. Eu volutpat odio facilisis mauris sit amet massa vitae. Senectus et netus et malesuada. Eget est lorem ipsum dolor sit amet consectetur. Posuere morbi leo urna molestie at elementum eu facilisis sed. Quisque id diam vel quam. Senectus et netus et malesuada fames ac turpis. A cras semper auctor neque vitae tempus quam. Id semper risus in hendrerit gravida rutrum quisque non tellus.\nMauris pellentesque pulvinar pellentesque habitant morbi tristique. Scelerisque in dictum non consectetur a erat nam. Senectus et netus et malesuada fames ac. Elementum nisi quis eleifend quam adipiscing vitae. Malesuada fames ac turpis egestas integer eget. Elit scelerisque mauris pellentesque pulvinar pellentesque habitant morbi tristique. Ut lectus arcu bibendum at. Adipiscing at in tellus integer feugiat scelerisque varius morbi. Sit amet aliquam id diam maecenas ultricies mi. Nam aliquam sem et tortor. Sociis natoque penatibus et magnis dis parturient. Adipiscing bibendum est ultricies integer. Pretium fusce id velit ut tortor pretium viverra suspendisse potenti. Lectus urna duis convallis convallis tellus id. Faucibus in ornare quam viverra orci sagittis eu volutpat. Viverra justo nec ultrices dui sapien.\nEget nunc lobortis mattis aliquam faucibus. Ipsum dolor sit amet consectetur adipiscing elit ut. Dui id ornare arcu odio ut sem. Commodo quis imperdiet massa tincidunt. Pellentesque sit amet porttitor eget dolor morbi. Auctor elit sed vulputate mi. Ut faucibus pulvinar elementum integer enim neque. Id porta nibh venenatis cras sed felis. Id eu nisl nunc mi ipsum faucibus. Malesuada fames ac turpis egestas integer eget aliquet nibh. Diam sollicitudin tempor id eu nisl nunc. Arcu cursus euismod quis viverra nibh cras pulvinar mattis nunc. Lorem ipsum dolor sit amet consectetur adipiscing elit pellentesque habitant. Viverra vitae congue eu consequat ac felis. Vitae et leo duis ut diam quam nulla."
  let width = 1200
  case maybeHelveticaBoldOblique of
    Just font ->
      return
        $ buildPdfDoc
        $ do
          producer "sample producer"
          creator "sample creator"
          creationDate now timeZone
          pageA4Landscape $ do
            forM_ (splitLines text' width font 24) $ \t -> do
              textHelvetica font $ do
                content $ "    " <> t
              moveDown
    _ -> return $ buildPdfDoc $
      do
        producer "sample producer"
        creator "sample creator"
  where
    pageA4Landscape :: PdfPageBuilder -> PdfDocumentBuilder
    pageA4Landscape =
      pageTemplate $ do
        pageSize sLetter
        layout landscape
    textHelvetica :: PdfStandardFont -> PdfTextBuilder -> PdfPageBuilder
    textHelvetica helvetica =
      textTemplate $ do
        font helvetica
        fontSize 8

getSamplePdfJsonR :: Handler Value
getSamplePdfJsonR = do
  pdfDoc <- samplePdfDoc
  let pdfDoc' = pdfDoc {_pdfDocumentHeaderLines = []}
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
