{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Pdf where

import Import
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B8
import Data.Time
import Text.Printf

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


-----------------------------------------------

encodePdf :: PdfDocument -> ByteString
encodePdf pdfDoc = B8.unlines $ toByteStringLines pdfDoc pdfDoc

encodePdf' :: PdfDocument -> [Text]
encodePdf' pdfDoc = map decodeUtf8 $ toByteStringLines pdfDoc pdfDoc

-----------------------------------------------

data PdfDocument = PdfDocument
  { pdfDocumentVersion :: Text
  , pdfDocumentHeaderLines :: [ByteString]
  , pdfDocumentCreationDate :: Text
  , pdfDocumentNextObjId :: Int
  , pdfDocumentInfo :: Maybe PdfInfo
  , pdfDocumentRoot :: PdfRoot
  , pdfDocumentPages :: PdfPages
  , pdfDocumentFont :: Maybe PdfFont
  , pdfDocumentResources :: Maybe PdfResources
  , pdfDocumentPage :: Maybe PdfPage
  , pdfDocumentTrailer :: PdfTrailer
  , pdfDocumentXref :: PdfXref
  , pdfDocumentStartXref :: Maybe Int
  }

pdfDocumentByteStringLineBlocks :: PdfDocument -> ([ByteString], [[ByteString]], [ByteString])
pdfDocumentByteStringLineBlocks pdfDoc =
  ( pdfDocumentHeaderLines pdfDoc
  , [
      ( toByteStringLines (pdfDocumentRoot pdfDoc) pdfDoc )
    , ( toByteStringLines (pdfDocumentPages pdfDoc) pdfDoc )
    , ( case pdfDocumentInfo pdfDoc of
           Just pdfInfo -> toByteStringLines pdfInfo pdfDoc
           _ -> []
       )
    , ( case pdfDocumentFont pdfDoc of
           Just pdfFont -> toByteStringLines pdfFont pdfDoc
           _ -> []
       )
    , ( case pdfDocumentResources pdfDoc of
           Just pdfResources -> toByteStringLines pdfResources pdfDoc
           _ -> []
       )
    , ( case pdfDocumentPage pdfDoc of
           Just pdfPage -> toByteStringLines pdfPage pdfDoc
           _ -> []
       )
    ]
  , ( toByteStringLines (pdfDocumentXref pdfDoc) pdfDoc )
    ++ ( toByteStringLines (pdfDocumentTrailer pdfDoc) pdfDoc )
    ++ [ encodeUtf8 "startxref"
       , encodeUtf8 $ maybeIntToText $ pdfDocumentStartXref pdfDoc
       , encodeUtf8 "%%EOF"
       ]
  )


instance ToJSON PdfDocument where
  toJSON o = object
    [ "version" .= pdfDocumentVersion o
    , "nextObjId" .= pdfDocumentNextObjId o
    , "info" .= pdfDocumentInfo o
    , "root" .= pdfDocumentRoot o
    , "pages" .= pdfDocumentPages o
    , "font" .= pdfDocumentFont o
    , "resources" .= pdfDocumentResources o
    , "page" .= pdfDocumentPage o
    , "trailer" .= pdfDocumentTrailer o
    , "xref" .= pdfDocumentXref o
    , "startxref" .= pdfDocumentStartXref o
    ]

instance ToByteStringLines PdfDocument where
  toByteStringLines pdfDoc _ =
    headerLines
    ++ L.foldl (\acc x -> acc ++ x) [] objectBlocks
    ++ footerLines
    where
      (headerLines, objectBlocks, footerLines) = pdfDocumentByteStringLineBlocks pdfDoc

data PdfInfo = PdfInfo
  { pdfInfoObjId :: Int
  , pdfInfoProducer :: Text
  , pdfInfoCreator :: Text
  , pdfInfoCreationDate :: Text
  }

instance ToJSON PdfInfo where
  toJSON o = object
    [ "objId" .= pdfInfoObjId o
    , "producer" .= pdfInfoProducer o
    , "creator" .= pdfInfoCreator o
    , "creationDate" .= pdfInfoCreationDate o
    ]

instance ToByteStringLines PdfInfo where
  toByteStringLines pdfInfo _ =
    [ encodeUtf8 $ (pack $ show $ pdfInfoObjId pdfInfo) ++ " 0 obj"
    , encodeUtf8 "% -------------- info"
    , encodeUtf8 "<<"
    , encodeUtf8 $ "/Producer (" ++ pdfInfoProducer pdfInfo ++ ")"
    , encodeUtf8 $ "/Creator (" ++ pdfInfoCreator pdfInfo ++ ")"
    , encodeUtf8 $ "/CreationDate (" ++ pdfInfoCreationDate pdfInfo ++ ")"
    , encodeUtf8 ">>"
    , encodeUtf8 "endobj"
    ]


data PdfRoot = PdfRoot
  { pdfRootObjId :: Int
  , pdfRootPages :: Text
  }

instance ToJSON PdfRoot where
  toJSON o = object
    [ "objId" .= pdfRootObjId o
    , "pages" .= pdfRootPages o
    ]

instance ToByteStringLines PdfRoot where
  toByteStringLines pdfRoot pdfDoc =
    [ encodeUtf8 $ (pack $ show $ pdfRootObjId pdfRoot) ++ " 0 obj"
    , encodeUtf8 "% -------------- root"
    , encodeUtf8 "<<"
    , encodeUtf8 $ "/Type /Catalog"
    , encodeUtf8 $ "/Pages " ++ (ref $ pdfPagesObjId $ pdfDocumentPages pdfDoc)
    , encodeUtf8 ">>"
    , encodeUtf8 "endobj"
    ]


data PdfPages = PdfPages
  { pdfPagesObjId :: Int
  , pdfPagesKids :: [Text]
  }

instance ToJSON PdfPages where
  toJSON o = object
    [ "objId" .= pdfPagesObjId o
    , "kids" .= pdfPagesKids o
    ]

instance ToByteStringLines PdfPages where
  toByteStringLines pdfPages pdfDoc =
    [ encodeUtf8 $ (pack $ show $ pdfPagesObjId pdfPages) ++ " 0 obj"
    , encodeUtf8 "% -------------- pages"
    , encodeUtf8 "<<"
    , encodeUtf8 $ "/Type /Pages"
    -- , encodeUtf8 $ "/Count " ++ (intToText . L.length $ pdfPagesKids pdfPages)
    -- , encodeUtf8 $ "/Kids []"
    , encodeUtf8 $ "/Count 1"
    , encodeUtf8 $ "/Kids [" ++ (case pdfDocumentPage pdfDoc of
                                   Just pdfPage -> ref $ pdfPageObjId $ pdfPage
                                   _ -> ""
                                ) ++ "]"
    , encodeUtf8 ">>"
    , encodeUtf8 "endobj"
    ]


data PdfFont = PdfFont
  { pdfFontObjId :: Int
  }

instance ToJSON PdfFont where
  toJSON o = object
    [ "objId" .= pdfFontObjId o
    ]

instance ToByteStringLines PdfFont where
  toByteStringLines pdfFont _ =
    [ encodeUtf8 $ (pack $ show $ pdfFontObjId pdfFont) ++ " 0 obj"
    , encodeUtf8 "% -------------- Font"
    , encodeUtf8 "<<"
    , encodeUtf8 $ "/Type /Font"
    , encodeUtf8 $ "/BaseFont /Helvetica"
    , encodeUtf8 $ "/Subtype /Type1"
    , encodeUtf8 $ "/Encoding /WinAnsiEncoding"
    , encodeUtf8 ">>"
    , encodeUtf8 "endobj"
    ]


data PdfResources = PdfResources
  { pdfResourcesObjId :: Int
  }

instance ToJSON PdfResources where
  toJSON o = object
    [ "objId" .= pdfResourcesObjId o
    ]

instance ToByteStringLines PdfResources where
  toByteStringLines pdfResources pdfDoc =
    [ encodeUtf8 $ (pack $ show $ pdfResourcesObjId pdfResources) ++ " 0 obj"
    , encodeUtf8 "% -------------- Resources"
    , encodeUtf8 "<<"
    , encodeUtf8 $ "/ProcSet [/PDF /Text /ImageB /ImageC /ImageI]"
    , encodeUtf8 $ "/Font <<"
    , encodeUtf8 $ "/F1 " ++ case pdfDocumentFont pdfDoc of
                               Just pdfFont -> ref $ pdfFontObjId pdfFont
                               _ -> ""
    , encodeUtf8 ">>"
    , encodeUtf8 ">>"
    , encodeUtf8 "endobj"
    ]


data PdfPage = PdfPage
  { pdfPageObjId :: Int
  }

instance ToJSON PdfPage where
  toJSON o = object
    [ "objId" .= pdfPageObjId o
    ]

instance ToByteStringLines PdfPage where
  toByteStringLines pdfPage pdfDoc =
    [ encodeUtf8 $ (pack $ show $ pdfPageObjId pdfPage) ++ " 0 obj"
    , encodeUtf8 "% -------------- Page"
    , encodeUtf8 "<<"
    , encodeUtf8 $ "/Type /Page"
    , encodeUtf8 $ "/Parent " ++ (ref $ pdfPagesObjId $ pdfDocumentPages pdfDoc)
    , encodeUtf8 $ "/MediaBox [0 0 612 792]"
    , encodeUtf8 $ "/Resources " ++ case pdfDocumentResources pdfDoc of
                               Just pdfResources -> ref $ pdfResourcesObjId pdfResources
                               _ -> ""
    , encodeUtf8 ">>"
    , encodeUtf8 ">>"
    , encodeUtf8 "endobj"
    ]


data PdfXref = PdfXref
  { pdfXrefPositions :: [Int]
  }

instance ToJSON PdfXref where
  toJSON o = object
    [ "positions" .= pdfXrefPositions o
    ]

instance ToByteStringLines PdfXref where
  toByteStringLines pdfXref pdfDoc =
    [ encodeUtf8 "xref"
    , encodeUtf8 "% -------------- xref"
    , encodeUtf8 $ "0 " ++ (intToText $ 1 + (pdfTrailerSize $ pdfDocumentTrailer pdfDoc))
    , encodeUtf8 $ "0000000000 65535 f"
    ]
    ++
    (L.map (\pos -> encodeUtf8 $ (formatXrefPos pos) ++ " 00000 n") $ pdfXrefPositions pdfXref)


data PdfTrailer = PdfTrailer
  { pdfTrailerSize :: Int
  , pdfTrailerInfo :: Maybe Text
  }

instance ToJSON PdfTrailer where
  toJSON o = object
    [ "size" .= pdfTrailerSize o
    , "info" .= pdfTrailerInfo o
    ]

instance ToByteStringLines PdfTrailer where
  toByteStringLines pdfTrailer pdfDoc =
    [ encodeUtf8 "trailer"
    , encodeUtf8 "% -------------- trailer"
    , encodeUtf8 "<<"
    , encodeUtf8 $ "/Size " ++ (intToText $ pdfTrailerSize pdfTrailer)
    , encodeUtf8 $ "/Root " ++ (ref $ pdfRootObjId $ pdfDocumentRoot pdfDoc)
    , encodeUtf8 $ "/Info " ++ (maybeTextToText $ pdfTrailerInfo pdfTrailer)
    , encodeUtf8 ">>"
    ]

-----------------------------------------------

info :: PdfBuilder
info = build ActionInfoSetup

infoProducer :: Text -> PdfBuilder
infoProducer text = build $ ActionInfoSetProducer text

infoCreator :: Text -> PdfBuilder
infoCreator text = build $ ActionInfoSetCreator text

finalize :: PdfBuilder
finalize = build ActionFinalize

font :: PdfBuilder
font = build ActionFont

resources :: PdfBuilder
resources = build ActionResources

page :: PdfBuilder
page = build ActionPage

data Action =
  ActionInfoSetup
  | ActionInfoSetProducer Text
  | ActionInfoSetCreator Text
  | ActionFinalize
  | ActionFont
  | ActionResources
  | ActionPage

instance IsExecutableAction Action where
  execute ActionInfoSetup pdfDoc =
    pdfDoc
    { pdfDocumentNextObjId = succ $ pdfDocumentNextObjId pdfDoc
    , pdfDocumentInfo =
        Just $ PdfInfo
        { pdfInfoObjId = pdfDocumentNextObjId pdfDoc
        , pdfInfoProducer = "hs-pdfkit"
        , pdfInfoCreator = "hs-pdfkit"
        , pdfInfoCreationDate = "D:" ++ pdfDocumentCreationDate pdfDoc ++ "Z"
        }
    , pdfDocumentTrailer =
      (pdfDocumentTrailer pdfDoc)
      { pdfTrailerSize = succ $ pdfTrailerSize (pdfDocumentTrailer pdfDoc)
      , pdfTrailerInfo = Just $ ref $ pdfDocumentNextObjId pdfDoc
      }
    }
  execute (ActionInfoSetProducer text) pdfDoc =
    pdfDoc
    { pdfDocumentInfo = case (pdfDocumentInfo pdfDoc) of
        Just pdfInfo -> Just $ pdfInfo { pdfInfoProducer = text}
        _ -> Nothing
    }
  execute (ActionInfoSetCreator text) pdfDoc =
    pdfDoc
    { pdfDocumentInfo = case (pdfDocumentInfo pdfDoc) of
        Just pdfInfo -> Just $ pdfInfo { pdfInfoCreator = text}
        _ -> Nothing
    }
  execute (ActionFinalize) pdfDoc =
    pdfDoc
    { pdfDocumentXref =
      PdfXref
      { pdfXrefPositions = L.init positions }
    , pdfDocumentStartXref = Just $ L.last positions
    }
    where
      (headerLines, objectBlocks, _) = pdfDocumentByteStringLineBlocks pdfDoc
      headerLength = length $ B8.unlines headerLines
      objectLines = L.map B8.unlines objectBlocks
      lengths = L.map length objectLines
      (positions, _) = L.foldl (\(ls,accl) len -> (ls ++ [accl+len], accl+len)) ([headerLength], headerLength) lengths
  execute (ActionFont) pdfDoc =
    pdfDoc
    { pdfDocumentNextObjId = succ $ pdfDocumentNextObjId pdfDoc
    , pdfDocumentFont =
        Just $ PdfFont
        { pdfFontObjId = pdfDocumentNextObjId pdfDoc
        }
    , pdfDocumentTrailer =
      (pdfDocumentTrailer pdfDoc)
      { pdfTrailerSize = succ $ pdfTrailerSize (pdfDocumentTrailer pdfDoc)
      }
    }
  execute (ActionResources) pdfDoc =
    pdfDoc
    { pdfDocumentNextObjId = succ $ pdfDocumentNextObjId pdfDoc
    , pdfDocumentResources =
        Just $ PdfResources
        { pdfResourcesObjId = pdfDocumentNextObjId pdfDoc
        }
    , pdfDocumentTrailer =
      (pdfDocumentTrailer pdfDoc)
      { pdfTrailerSize = succ $ pdfTrailerSize (pdfDocumentTrailer pdfDoc)
      }
    }
  execute (ActionPage) pdfDoc =
    pdfDoc
    { pdfDocumentNextObjId = succ $ pdfDocumentNextObjId pdfDoc
    , pdfDocumentPage =
        Just $ PdfPage
        { pdfPageObjId = pdfDocumentNextObjId pdfDoc
        }
    , pdfDocumentTrailer =
      (pdfDocumentTrailer pdfDoc)
      { pdfTrailerSize = succ $ pdfTrailerSize (pdfDocumentTrailer pdfDoc)
      }
    }


-----------------------------------------------

build :: Action -> PdfBuilder
build action = PdfBuilderM () [action]

run :: Text -> PdfBuilderM b -> PdfDocument
run creationDate (PdfBuilderM _ actions) =
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
    , pdfDocumentResources = Nothing
    , pdfDocumentPage = Nothing
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
    version = "1.3"
    rootObjId = 1
    pagesObjId = 2
    nextObjId = 3

class ToByteStringLines b where
  toByteStringLines :: b -> PdfDocument -> [ByteString]

class IsExecutableAction a where
  execute :: a -> PdfDocument -> PdfDocument

data PdfBuilderM b = PdfBuilderM b [Action]

type PdfBuilder = PdfBuilderM ()

instance Functor PdfBuilderM where
  fmap = liftM

instance Applicative PdfBuilderM where
  pure  = return
  (<*>) = ap

instance Monad PdfBuilderM where
  return b = PdfBuilderM b []
  PdfBuilderM b actions1 >>= f =
    let PdfBuilderM b2 actions2 = f b
    in  PdfBuilderM b2 (actions1 ++ actions2)

-----------------------------------------------

formatLocalTime :: TimeZone -> UTCTime -> String
formatLocalTime timeZone utcTime = formatTime defaultTimeLocale "%Y%m%d%H%M%S" $ utcToLocalTime timeZone utcTime

maybeTextToText :: Maybe Text -> Text
maybeTextToText Nothing = ""
maybeTextToText (Just t) = t

maybeIntToText :: Maybe Int -> Text
maybeIntToText Nothing = ""
maybeIntToText (Just i) = pack . show $ i

intToText :: Int -> Text
intToText i = pack . show $ i

ref :: Int -> Text
ref objId = intToText objId ++ " 0 R"

formatXrefPos :: Int -> Text
formatXrefPos i = pack $ printf "%010d" i

currentTimeZone :: Handler TimeZone
currentTimeZone = do
  timeZone <- liftIO $ getCurrentTimeZone
  return timeZone
