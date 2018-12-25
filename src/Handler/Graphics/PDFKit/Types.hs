{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Graphics.PDFKit.Types where

import Import
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B8
import Handler.Graphics.PDFKit.Helpers


data PdfDocument = PdfDocument
  { pdfDocumentVersion :: Text
  , pdfDocumentHeaderLines :: [ByteString]
  , pdfDocumentCreationDate :: Text
  , pdfDocumentNextObjId :: Int
  , pdfDocumentInfo :: Maybe PdfInfo
  , pdfDocumentRoot :: PdfRoot
  , pdfDocumentPages :: PdfPages
  , pdfDocumentFont :: Maybe PdfFont
  , pdfDocumentPage :: Maybe PdfPage
  , pdfDocumentTrailer :: PdfTrailer
  , pdfDocumentXref :: PdfXref
  , pdfDocumentStartXref :: Maybe Int
  }

instance ToByteStringLines PdfDocument where
  toByteStringLines pdfDoc _ =
    headerLines
    ++ L.foldl (\acc x -> acc ++ x) [] objectBlocks
    ++ footerLines
    where
      (headerLines, objectBlocks, footerLines) = pdfDocumentByteStringLineBlocks pdfDoc

instance ToJSON PdfDocument where
  toJSON o = object
    [ "version" .= pdfDocumentVersion o
    , "nextObjId" .= pdfDocumentNextObjId o
    , "info" .= pdfDocumentInfo o
    , "root" .= pdfDocumentRoot o
    , "pages" .= pdfDocumentPages o
    , "font" .= pdfDocumentFont o
    , "page" .= pdfDocumentPage o
    , "trailer" .= pdfDocumentTrailer o
    , "xref" .= pdfDocumentXref o
    , "startxref" .= pdfDocumentStartXref o
    ]

-----------------------------------------------

data PdfInfo = PdfInfo
  { pdfInfoObjId :: Int
  , pdfInfoProducer :: Text
  , pdfInfoCreator :: Text
  , pdfInfoCreationDate :: Text
  }

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

instance ToJSON PdfInfo where
  toJSON o = object
    [ "objId" .= pdfInfoObjId o
    , "producer" .= pdfInfoProducer o
    , "creator" .= pdfInfoCreator o
    , "creationDate" .= pdfInfoCreationDate o
    ]

-----------------------------------------------

data PdfRoot = PdfRoot
  { pdfRootObjId :: Int
  , pdfRootPages :: Text
  }

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

instance ToJSON PdfRoot where
  toJSON o = object
    [ "objId" .= pdfRootObjId o
    , "pages" .= pdfRootPages o
    ]

-----------------------------------------------

data PdfPages = PdfPages
  { pdfPagesObjId :: Int
  , pdfPagesKids :: [Text]
  }

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

instance ToJSON PdfPages where
  toJSON o = object
    [ "objId" .= pdfPagesObjId o
    , "kids" .= pdfPagesKids o
    ]

-----------------------------------------------

data PdfFont = PdfFont
  { pdfFontObjId :: Int
  }

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

instance ToJSON PdfFont where
  toJSON o = object
    [ "objId" .= pdfFontObjId o
    ]

-----------------------------------------------

data PdfResources = PdfResources
  { pdfResourcesObjId :: Int
  }

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

instance ToJSON PdfResources where
  toJSON o = object
    [ "objId" .= pdfResourcesObjId o
    ]

-----------------------------------------------

data PdfPage = PdfPage
  { pdfPageObjId :: Int
  , pdfPageResources :: Maybe PdfResources
  }

instance ToByteStringLines PdfPage where
  toByteStringLines pdfPage pdfDoc =
    [ encodeUtf8 $ (pack $ show $ pdfPageObjId pdfPage) ++ " 0 obj"
    , encodeUtf8 "% -------------- Page"
    , encodeUtf8 "<<"
    , encodeUtf8 $ "/Type /Page"
    , encodeUtf8 $ "/Parent " ++ (ref $ pdfPagesObjId $ pdfDocumentPages pdfDoc)
    , encodeUtf8 $ "/MediaBox [0 0 612 792]"
    , encodeUtf8 $ "/Resources " ++ case pdfPageResources pdfPage of
                                      Just pdfResources -> (ref $ pdfResourcesObjId pdfResources)
                                      _ -> ""
    , encodeUtf8 ">>"
    , encodeUtf8 ">>"
    , encodeUtf8 "endobj"
    ]

instance ToJSON PdfPage where
  toJSON o = object
    [ "objId" .= pdfPageObjId o
    , "resources" .= pdfPageResources o
    ]

-----------------------------------------------

data PdfXref = PdfXref
  { pdfXrefPositions :: [Int]
  }

instance ToByteStringLines PdfXref where
  toByteStringLines pdfXref pdfDoc =
    [ encodeUtf8 "xref"
    , encodeUtf8 "% -------------- xref"
    , encodeUtf8 $ "0 " ++ (intToText $ 1 + (pdfTrailerSize $ pdfDocumentTrailer pdfDoc))
    , encodeUtf8 $ "0000000000 65535 f"
    ]
    ++
    (L.map (\pos -> encodeUtf8 $ (formatXrefPos pos) ++ " 00000 n") $ pdfXrefPositions pdfXref)

instance ToJSON PdfXref where
  toJSON o = object
    [ "positions" .= pdfXrefPositions o
    ]

-----------------------------------------------

data PdfTrailer = PdfTrailer
  { pdfTrailerSize :: Int
  , pdfTrailerInfo :: Maybe Text
  }

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

instance ToJSON PdfTrailer where
  toJSON o = object
    [ "size" .= pdfTrailerSize o
    , "info" .= pdfTrailerInfo o
    ]

-----------------------------------------------

data Action =
  ActionInfoSetup
  | ActionInfoSetProducer Text
  | ActionInfoSetCreator Text
  | ActionFinalize
  | ActionFont
  | ActionResources
  | ActionPage

build :: Action -> PdfBuilder
build action = PdfBuilderM () [action]

-----------------------------------------------

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
    , ( case pdfDocumentPage pdfDoc of
           Just pdfPage ->
             toByteStringLines pdfPage pdfDoc
             ++ case pdfPageResources pdfPage of
                  Just pdfResources -> toByteStringLines pdfResources pdfDoc
                  _ -> []
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
  execute (ActionPage) pdfDoc =
    pdfDoc
    { pdfDocumentNextObjId = succ $ pdfDocumentNextObjId pdfDoc
    , pdfDocumentPage =
        Just $ PdfPage
        { pdfPageObjId = pdfDocumentNextObjId pdfDoc
        , pdfPageResources = Nothing
        }
    , pdfDocumentTrailer =
      (pdfDocumentTrailer pdfDoc)
      { pdfTrailerSize = succ $ pdfTrailerSize (pdfDocumentTrailer pdfDoc)
      }
    }
  execute (ActionResources) pdfDoc =
    pdfDoc
    { pdfDocumentNextObjId = succ $ pdfDocumentNextObjId pdfDoc
    , pdfDocumentPage =
        case (pdfDocumentPage pdfDoc) of
          Just pdfPage ->
            Just $ pdfPage
            { pdfPageResources =
              Just $ PdfResources
              { pdfResourcesObjId = pdfDocumentNextObjId pdfDoc
              }
            }
          _ -> Nothing
    , pdfDocumentTrailer =
      (pdfDocumentTrailer pdfDoc)
      { pdfTrailerSize = succ $ pdfTrailerSize (pdfDocumentTrailer pdfDoc)
      }
    }

-----------------------------------------------

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
