{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Graphics.PDFKit.PDFKit where

import Import
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B8
import Handler.Graphics.PDFKit.Types
import Handler.Graphics.PDFKit.Helpers


info :: PdfBuilder
info = build ActionInfoSetup

infoProducer :: Text -> PdfBuilder
infoProducer text = build $ ActionInfoSetProducer text

infoCreator :: Text -> PdfBuilder
infoCreator text = build $ ActionInfoSetCreator text

font :: PdfBuilder
font = build ActionFont

page :: PdfBuilder
page = do
  build ActionPage
  build ActionResources

pageSize :: PdfPageSize -> PdfBuilder
pageSize size = build $ ActionPageSetSize size

pageLayout :: PdfPageLayout -> PdfBuilder
pageLayout lay = build $ ActionPageSetLayout lay

-----------------------------------------------

run :: Text -> PdfBuilderM b -> PdfDocument
run creationDate (PdfBuilderM _ userActions) =
  L.foldl
  (\pdfDoc action -> execute action pdfDoc)
  ( PdfDocument
    { pdfDocumentVersion = version
    , pdfDocumentHeaderLines = [ encodeUtf8 $ "%PDF-" ++ version
                               , "%" ++ B8.pack ['\xff', '\xff', '\xff', '\xff']
                               ]
    , pdfDocumentCreationDate = creationDate
    , pdfDocumentNextObjId = nextObjId
    , pdfDocumentInfo = Nothing
    , pdfDocumentRoot =
      PdfRoot
      { pdfRootObjId = rootObjId
      , pdfRootPages = ref pagesObjId
      }
    , pdfDocumentPages =
      PdfPages
      { pdfPagesObjId = pagesObjId
      , pdfPagesKids = []
      }
    , pdfDocumentFont = Nothing
    , pdfDocumentXref =
      PdfXref
      { pdfXrefPositions = []
      }
    , pdfDocumentTrailer =
      PdfTrailer
      { pdfTrailerSize = pred nextObjId
      , pdfTrailerInfo = Nothing
      }
    , pdfDocumentStartXref = Nothing
    }
  )
  actions
  where
    actions = userActions ++ [ActionFinalize]
    version = "1.3"
    rootObjId = 1
    pagesObjId = 2
    nextObjId = 3
