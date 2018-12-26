{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Graphics.PDFKit.Pdf where

import Import
import qualified Data.List as L
import qualified Data.Text as T
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
  , pdfDocumentTrailer :: PdfTrailer
  , pdfDocumentXref :: PdfXref
  , pdfDocumentStartXref :: Maybe Int
  }

initialPdfDocument :: Text -> PdfDocument
initialPdfDocument creationDate =
  PdfDocument
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
  where
    version = "1.3"
    rootObjId = 1
    pagesObjId = 2
    nextObjId = 3

instance ToByteStringLines PdfDocument where
  toByteStringLines pdfDoc _ =
    headerLines
    ++ L.foldl (\acc x -> acc ++ x) [] objectBlocks
    ++ footerLines
    where
      (headerLines, objectBlocks, footerLines) =
        pdfDocumentByteStringLineBlocks pdfDoc

instance ToJSON PdfDocument where
  toJSON o = object
    [ "version" .= pdfDocumentVersion o
    , "nextObjId" .= pdfDocumentNextObjId o
    , "info" .= pdfDocumentInfo o
    , "root" .= pdfDocumentRoot o
    , "pages" .= pdfDocumentPages o
    , "font" .= pdfDocumentFont o
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
    , encodeUtf8 $ "% ------------------------------------------------------ info " ++ (intToText $ pdfInfoObjId pdfInfo)
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
    , encodeUtf8 $ "% ------------------------------------------------------ root " ++ (intToText $ pdfRootObjId pdfRoot)
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
  , pdfPagesKids :: [PdfPage]
  }

instance ToByteStringLines PdfPages where
  toByteStringLines pdfPages _ =
    [ encodeUtf8 $ (pack $ show $ pdfPagesObjId pdfPages) ++ " 0 obj"
    , encodeUtf8 $ "% ------------------------------------------------------ pages " ++ (intToText $ pdfPagesObjId pdfPages)
    , encodeUtf8 "<<"
    , encodeUtf8 $ "/Type /Pages"
    , encodeUtf8 $ "/Count " ++ (intToText $ length $ pdfPagesKids pdfPages)
    , encodeUtf8 $ "/Kids [" ++ (T.intercalate "  " $
                                 L.map (\pdfPage -> ref $ pdfPageObjId pdfPage) (pdfPagesKids pdfPages)
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
    , encodeUtf8 $ "% ------------------------------------------------------ Font " ++ (intToText $ pdfFontObjId pdfFont)
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
    , encodeUtf8 $ "% ------------------------------------------------------ Resources " ++ (intToText $ pdfResourcesObjId pdfResources)
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
  , pdfPageSize :: PdfPageSize
  , pdfPageMargins :: PdfPageMargins
  , pdfPageLayout :: PdfPageLayout
  , pdfPageResources :: Maybe PdfResources
  }

instance ToByteStringLines PdfPage where
  toByteStringLines pdfPage pdfDoc =
    [ encodeUtf8 $ (pack $ show $ pdfPageObjId pdfPage) ++ " 0 obj"
    , encodeUtf8 $ "% ------------------------------------------------------ Page " ++ (intToText $ pdfPageObjId pdfPage)
    , encodeUtf8 "<<"
    , encodeUtf8 $ "/Type /Page"
    , encodeUtf8 $ "/Parent " ++ (ref $ pdfPagesObjId $ pdfDocumentPages pdfDoc)
    , encodeUtf8 $ "% " ++ (pack $ show $ pdfPageLayout pdfPage)
    , encodeUtf8 $ "/MediaBox [0 0 "
      ++ (doubleToText $ pdfPageSizeWidth pageSize) ++ " "
      ++ (doubleToText $ pdfPageSizeHeight pageSize) ++ "]"
    , encodeUtf8 $ "/Resources " ++ case pdfPageResources pdfPage of
                                      Just pdfResources -> (ref $ pdfResourcesObjId pdfResources)
                                      _ -> ""
    , encodeUtf8 ">>"
    , encodeUtf8 ">>"
    , encodeUtf8 "endobj"
    ]
    where
      pageSize = applyLayout (pdfPageSize pdfPage) (pdfPageLayout pdfPage)

instance ToJSON PdfPage where
  toJSON o = object
    [ "objId" .= pdfPageObjId o
    , "size" .= pdfPageSize o
    , "margins" .= pdfPageMargins o
    , "layout" .= ( T.pack $ case pdfPageLayout o of
                               Portrait -> "portrait"
                               Landscape -> "landscape" )
    , "resources" .= pdfPageResources o
    ]

-----------------------------------------------

data PdfXref = PdfXref
  { pdfXrefPositions :: [Int]
  }

instance ToByteStringLines PdfXref where
  toByteStringLines pdfXref pdfDoc =
    [ encodeUtf8 "xref"
    , encodeUtf8 "% ------------------------------------------------------ xref"
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
    , encodeUtf8 "% ------------------------------------------------------ trailer"
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

data PdfPageMargins =
  PdfPageMargins
  { pdfPageMarginTop :: Double
  , pdfPageMarginLeft :: Double
  , pdfPageMarginBottom :: Double
  , pdfPageMarginRight :: Double
  }

defaultPageMargins :: PdfPageMargins
defaultPageMargins = PdfPageMargins 72 72 72 72

instance ToJSON PdfPageMargins where
  toJSON o = object
    [ "top" .= pdfPageMarginTop o
    , "left" .= pdfPageMarginLeft o
    , "bottom" .= pdfPageMarginBottom o
    , "right" .= pdfPageMarginRight o
    ]

-----------------------------------------------

data PdfPageLayout = Portrait | Landscape
  deriving Show

portrait :: PdfPageLayout
portrait = Portrait

landscape :: PdfPageLayout
landscape = Landscape

-----------------------------------------------

data PdfPageSize = PdfPageSize
  { pdfPageSizeWidth :: Double
  , pdfPageSizeHeight :: Double
  }

instance ToJSON PdfPageSize where
  toJSON o = object
    [ "width" .= pdfPageSizeWidth o
    , "height" .= pdfPageSizeHeight o
    ]

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

-----------------------------------------------

pdfPagesTuple :: PdfDocument -> (PdfPages, [PdfPage], PdfPage)
pdfPagesTuple pdfDoc = (pdfPages, initPages, lastPage)
  where
    pdfPages = pdfDocumentPages pdfDoc
    initPages = L.init $ pdfPagesKids pdfPages
    lastPage = L.last $ pdfPagesKids pdfPages

applyLayout :: PdfPageSize -> PdfPageLayout -> PdfPageSize
applyLayout size Portrait = size
applyLayout (PdfPageSize {pdfPageSizeWidth = w, pdfPageSizeHeight = h }) Landscape =
  PdfPageSize { pdfPageSizeWidth = h
              , pdfPageSizeHeight = w
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
    ]
    ++ ( L.map (\pdfPage ->
                  toByteStringLines pdfPage pdfDoc
                  ++ case pdfPageResources pdfPage of
                       Just pdfResources -> toByteStringLines pdfResources pdfDoc
                       _ -> []
               )
         (pdfPagesKids $ pdfDocumentPages pdfDoc)
       )
  , ( toByteStringLines (pdfDocumentXref pdfDoc) pdfDoc )
    ++ ( toByteStringLines (pdfDocumentTrailer pdfDoc) pdfDoc )
    ++ [ encodeUtf8 "startxref"
       , encodeUtf8 $ maybeIntToText $ pdfDocumentStartXref pdfDoc
       , encodeUtf8 "%%EOF"
       ]
  )

size4A0 :: PdfPageSize
size4A0 = PdfPageSize 4767.87 6740.79
size2A0 :: PdfPageSize
size2A0 = PdfPageSize 3370.39 4767.87
sizeA0 :: PdfPageSize
sizeA0 = PdfPageSize 2383.94 3370.39
sizeA1 :: PdfPageSize
sizeA1 = PdfPageSize 1683.78 2383.94
sizeA2 :: PdfPageSize
sizeA2 = PdfPageSize 1190.55 1683.78
sizeA3 :: PdfPageSize
sizeA3 = PdfPageSize 841.89 1190.55
sizeA4 :: PdfPageSize
sizeA4 = PdfPageSize 595.28 841.89
sizeA5 :: PdfPageSize
sizeA5 = PdfPageSize 419.53 595.28
sizeA6 :: PdfPageSize
sizeA6 = PdfPageSize 297.64 419.53
sizeA7 :: PdfPageSize
sizeA7 = PdfPageSize 209.76 297.64
sizeA8 :: PdfPageSize
sizeA8 = PdfPageSize 147.40 209.76
sizeA9 :: PdfPageSize
sizeA9 = PdfPageSize 104.88 147.40
sizeA10 :: PdfPageSize
sizeA10 = PdfPageSize 73.70 104.88
sizeB0 :: PdfPageSize
sizeB0 = PdfPageSize 2834.65 4008.19
sizeB1 :: PdfPageSize
sizeB1 = PdfPageSize 2004.09 2834.65
sizeB2 :: PdfPageSize
sizeB2 = PdfPageSize 1417.32 2004.09
sizeB3 :: PdfPageSize
sizeB3 = PdfPageSize 1000.63 1417.32
sizeB4 :: PdfPageSize
sizeB4 = PdfPageSize 708.66 1000.63
sizeB5 :: PdfPageSize
sizeB5 = PdfPageSize 498.90 708.66
sizeB6 :: PdfPageSize
sizeB6 = PdfPageSize 354.33 498.90
sizeB7 :: PdfPageSize
sizeB7 = PdfPageSize 249.45 354.33
sizeB8 :: PdfPageSize
sizeB8 = PdfPageSize 175.75 249.45
sizeB9 :: PdfPageSize
sizeB9 = PdfPageSize 124.72 175.75
sizeB10 :: PdfPageSize
sizeB10 = PdfPageSize 87.87 124.72
sizeC0 :: PdfPageSize
sizeC0 = PdfPageSize 2599.37 3676.54
sizeC1 :: PdfPageSize
sizeC1 = PdfPageSize 1836.85 2599.37
sizeC2 :: PdfPageSize
sizeC2 = PdfPageSize 1298.27 1836.85
sizeC3 :: PdfPageSize
sizeC3 = PdfPageSize 918.43 1298.27
sizeC4 :: PdfPageSize
sizeC4 = PdfPageSize 649.13 918.43
sizeC5 :: PdfPageSize
sizeC5 = PdfPageSize 459.21 649.13
sizeC6 :: PdfPageSize
sizeC6 = PdfPageSize 323.15 459.21
sizeC7 :: PdfPageSize
sizeC7 = PdfPageSize 229.61 323.15
sizeC8 :: PdfPageSize
sizeC8 = PdfPageSize 161.57 229.61
sizeC9 :: PdfPageSize
sizeC9 = PdfPageSize 113.39 161.57
sizeC10 :: PdfPageSize
sizeC10 = PdfPageSize 79.37 113.39
sizeRA0 :: PdfPageSize
sizeRA0 = PdfPageSize 2437.80 3458.27
sizeRA1 :: PdfPageSize
sizeRA1 = PdfPageSize 1729.13 2437.80
sizeRA2 :: PdfPageSize
sizeRA2 = PdfPageSize 1218.90 1729.13
sizeRA3 :: PdfPageSize
sizeRA3 = PdfPageSize 864.57 1218.90
sizeRA4 :: PdfPageSize
sizeRA4 = PdfPageSize 609.45 864.57
sizeSRA0 :: PdfPageSize
sizeSRA0 = PdfPageSize 2551.18 3628.35
sizeSRA1 :: PdfPageSize
sizeSRA1 = PdfPageSize 1814.17 2551.18
sizeSRA2 :: PdfPageSize
sizeSRA2 = PdfPageSize 1275.59 1814.17
sizeSRA3 :: PdfPageSize
sizeSRA3 = PdfPageSize 907.09 1275.59
sizeSRA4 :: PdfPageSize
sizeSRA4 = PdfPageSize 637.80 907.09
sizeEXECUTIVE :: PdfPageSize
sizeEXECUTIVE = PdfPageSize 521.86 756.00
sizeFOLIO :: PdfPageSize
sizeFOLIO = PdfPageSize 612.00 936.00
sizeLEGAL :: PdfPageSize
sizeLEGAL = PdfPageSize 612.00 1008.00
sizeLETTER :: PdfPageSize
sizeLETTER = PdfPageSize 612.00 792.00
sizeTABLOID :: PdfPageSize
sizeTABLOID = PdfPageSize 792.00 1224.00

-----------------------------------------------

data Action =
  ActionInfoSetup
  | ActionInfoSetProducer Text
  | ActionInfoSetCreator Text
  | ActionFinalize
  | ActionFont
  | ActionResources
  | ActionPage
  | ActionPageSetSize PdfPageSize
  | ActionPageSetLayout PdfPageLayout
  | ActionPageSetMargin Double
  | ActionPageSetMargins Double Double Double Double
  | ActionPageSetSizeCustom Double Double

build :: Action -> PdfBuilder
build action = PdfBuilderM () [action]

-----------------------------------------------

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
    , pdfDocumentPages =
      (pdfDocumentPages pdfDoc)
      { pdfPagesKids =
          (pdfPagesKids $ pdfDocumentPages pdfDoc)
          ++
          [ PdfPage
            { pdfPageObjId = pdfDocumentNextObjId pdfDoc
            , pdfPageSize = sizeA4
            , pdfPageMargins = defaultPageMargins
            , pdfPageLayout = Portrait
            , pdfPageResources = Nothing
            }
          ]
      }
    , pdfDocumentTrailer =
      (pdfDocumentTrailer pdfDoc)
      { pdfTrailerSize = succ $ pdfTrailerSize (pdfDocumentTrailer pdfDoc)
      }
    }

  execute (ActionPageSetSize size) pdfDoc =
    pdfDoc
    { pdfDocumentPages =
      pdfPages
      { pdfPagesKids =
        initPages
        ++
        [ lastPage
          { pdfPageSize = size
          }
        ]
      }
    }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc

  execute (ActionPageSetSizeCustom width height) pdfDoc =
    pdfDoc
    { pdfDocumentPages =
      pdfPages
      { pdfPagesKids =
        initPages
        ++
        [ lastPage
          { pdfPageSize =
            PdfPageSize
            { pdfPageSizeWidth = width
            , pdfPageSizeHeight = height
            }
          }
        ]
      }
    }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc

  execute (ActionPageSetLayout lay) pdfDoc =
    pdfDoc
    { pdfDocumentPages =
      pdfPages
      { pdfPagesKids =
        initPages
        ++
        [ lastPage
          { pdfPageLayout = lay
          }
        ]
      }
    }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc

  execute (ActionPageSetMargin x) pdfDoc =
    pdfDoc
    { pdfDocumentPages =
      pdfPages
      { pdfPagesKids =
        initPages
        ++
        [ lastPage
          { pdfPageMargins = PdfPageMargins x x x x
          }
        ]
      }
    }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc

  execute (ActionPageSetMargins top left bottom right) pdfDoc =
    pdfDoc
    { pdfDocumentPages =
      pdfPages
      { pdfPagesKids =
        initPages
        ++
        [ lastPage
          { pdfPageMargins =
            PdfPageMargins top left bottom right
          }
        ]
      }
    }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc

  execute (ActionResources) pdfDoc =
    pdfDoc
    { pdfDocumentNextObjId = succ $ pdfDocumentNextObjId pdfDoc
    , pdfDocumentPages =
      pdfPages
      { pdfPagesKids =
        initPages
        ++
        [ lastPage
          { pdfPageResources =
              Just $ PdfResources
              { pdfResourcesObjId = pdfDocumentNextObjId pdfDoc
              }
          }
        ]
      }
    , pdfDocumentTrailer =
      (pdfDocumentTrailer pdfDoc)
      { pdfTrailerSize = succ $ pdfTrailerSize (pdfDocumentTrailer pdfDoc)
      }
    }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc
