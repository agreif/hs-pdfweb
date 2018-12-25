{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Graphics.PDFKit.Types where

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
    , encodeUtf8 $ "/MediaBox [0 0 "
      ++ (doubleToText $ pdfPageSizeWidth $ pdfPageSize pdfPage) ++ " "
      ++ (doubleToText $ pdfPageSizeHeight $ pdfPageSize pdfPage) ++ "]"
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
    , "size" .= pdfPageSize o
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
            , pdfPageSize = pdfPageSizeA4
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
  execute (ActionResources) pdfDoc =
    pdfDoc
    { pdfDocumentNextObjId = succ $ pdfDocumentNextObjId pdfDoc
    , pdfDocumentPages =
      pdfPages
      { pdfPagesKids =
        prevPages
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
      pdfPages = pdfDocumentPages pdfDoc
      prevPages = L.init $ pdfPagesKids pdfPages
      lastPage = L.last $ pdfPagesKids pdfPages

-----------------------------------------------

data PdfPageLayout = Portrait | Landscape

data PdfPageSize = PdfPageSize
  { pdfPageSizeWidth :: Double
  , pdfPageSizeHeight :: Double
  }

instance ToJSON PdfPageSize where
  toJSON o = object
    [ "width" .= pdfPageSizeWidth o
    , "height" .= pdfPageSizeHeight o
    ]

pdfPageSize4A0 :: PdfPageSize
pdfPageSize4A0 = PdfPageSize 4767.87 6740.79
pdfPageSize2A0 :: PdfPageSize
pdfPageSize2A0 = PdfPageSize 3370.39 4767.87
pdfPageSizeA0 :: PdfPageSize
pdfPageSizeA0 = PdfPageSize 2383.94 3370.39
pdfPageSizeA1 :: PdfPageSize
pdfPageSizeA1 = PdfPageSize 1683.78 2383.94
pdfPageSizeA2 :: PdfPageSize
pdfPageSizeA2 = PdfPageSize 1190.55 1683.78
pdfPageSizeA3 :: PdfPageSize
pdfPageSizeA3 = PdfPageSize 841.89 1190.55
pdfPageSizeA4 :: PdfPageSize
pdfPageSizeA4 = PdfPageSize 595.28 841.89
pdfPageSizeA5 :: PdfPageSize
pdfPageSizeA5 = PdfPageSize 419.53 595.28
pdfPageSizeA6 :: PdfPageSize
pdfPageSizeA6 = PdfPageSize 297.64 419.53
pdfPageSizeA7 :: PdfPageSize
pdfPageSizeA7 = PdfPageSize 209.76 297.64
pdfPageSizeA8 :: PdfPageSize
pdfPageSizeA8 = PdfPageSize 147.40 209.76
pdfPageSizeA9 :: PdfPageSize
pdfPageSizeA9 = PdfPageSize 104.88 147.40
pdfPageSizeA10 :: PdfPageSize
pdfPageSizeA10 = PdfPageSize 73.70 104.88
pdfPageSizeB0 :: PdfPageSize
pdfPageSizeB0 = PdfPageSize 2834.65 4008.19
pdfPageSizeB1 :: PdfPageSize
pdfPageSizeB1 = PdfPageSize 2004.09 2834.65
pdfPageSizeB2 :: PdfPageSize
pdfPageSizeB2 = PdfPageSize 1417.32 2004.09
pdfPageSizeB3 :: PdfPageSize
pdfPageSizeB3 = PdfPageSize 1000.63 1417.32
pdfPageSizeB4 :: PdfPageSize
pdfPageSizeB4 = PdfPageSize 708.66 1000.63
pdfPageSizeB5 :: PdfPageSize
pdfPageSizeB5 = PdfPageSize 498.90 708.66
pdfPageSizeB6 :: PdfPageSize
pdfPageSizeB6 = PdfPageSize 354.33 498.90
pdfPageSizeB7 :: PdfPageSize
pdfPageSizeB7 = PdfPageSize 249.45 354.33
pdfPageSizeB8 :: PdfPageSize
pdfPageSizeB8 = PdfPageSize 175.75 249.45
pdfPageSizeB9 :: PdfPageSize
pdfPageSizeB9 = PdfPageSize 124.72 175.75
pdfPageSizeB10 :: PdfPageSize
pdfPageSizeB10 = PdfPageSize 87.87 124.72
pdfPageSizeC0 :: PdfPageSize
pdfPageSizeC0 = PdfPageSize 2599.37 3676.54
pdfPageSizeC1 :: PdfPageSize
pdfPageSizeC1 = PdfPageSize 1836.85 2599.37
pdfPageSizeC2 :: PdfPageSize
pdfPageSizeC2 = PdfPageSize 1298.27 1836.85
pdfPageSizeC3 :: PdfPageSize
pdfPageSizeC3 = PdfPageSize 918.43 1298.27
pdfPageSizeC4 :: PdfPageSize
pdfPageSizeC4 = PdfPageSize 649.13 918.43
pdfPageSizeC5 :: PdfPageSize
pdfPageSizeC5 = PdfPageSize 459.21 649.13
pdfPageSizeC6 :: PdfPageSize
pdfPageSizeC6 = PdfPageSize 323.15 459.21
pdfPageSizeC7 :: PdfPageSize
pdfPageSizeC7 = PdfPageSize 229.61 323.15
pdfPageSizeC8 :: PdfPageSize
pdfPageSizeC8 = PdfPageSize 161.57 229.61
pdfPageSizeC9 :: PdfPageSize
pdfPageSizeC9 = PdfPageSize 113.39 161.57
pdfPageSizeC10 :: PdfPageSize
pdfPageSizeC10 = PdfPageSize 79.37 113.39
pdfPageSizeRA0 :: PdfPageSize
pdfPageSizeRA0 = PdfPageSize 2437.80 3458.27
pdfPageSizeRA1 :: PdfPageSize
pdfPageSizeRA1 = PdfPageSize 1729.13 2437.80
pdfPageSizeRA2 :: PdfPageSize
pdfPageSizeRA2 = PdfPageSize 1218.90 1729.13
pdfPageSizeRA3 :: PdfPageSize
pdfPageSizeRA3 = PdfPageSize 864.57 1218.90
pdfPageSizeRA4 :: PdfPageSize
pdfPageSizeRA4 = PdfPageSize 609.45 864.57
pdfPageSizeSRA0 :: PdfPageSize
pdfPageSizeSRA0 = PdfPageSize 2551.18 3628.35
pdfPageSizeSRA1 :: PdfPageSize
pdfPageSizeSRA1 = PdfPageSize 1814.17 2551.18
pdfPageSizeSRA2 :: PdfPageSize
pdfPageSizeSRA2 = PdfPageSize 1275.59 1814.17
pdfPageSizeSRA3 :: PdfPageSize
pdfPageSizeSRA3 = PdfPageSize 907.09 1275.59
pdfPageSizeSRA4 :: PdfPageSize
pdfPageSizeSRA4 = PdfPageSize 637.80 907.09
pdfPageSizeEXECUTIVE :: PdfPageSize
pdfPageSizeEXECUTIVE = PdfPageSize 521.86 756.00
pdfPageSizeFOLIO :: PdfPageSize
pdfPageSizeFOLIO = PdfPageSize 612.00 936.00
pdfPageSizeLEGAL :: PdfPageSize
pdfPageSizeLEGAL = PdfPageSize 612.00 1008.00
pdfPageSizeLETTER :: PdfPageSize
pdfPageSizeLETTER = PdfPageSize 612.00 792.00
pdfPageSizeTABLOID :: PdfPageSize
pdfPageSizeTABLOID = PdfPageSize 792.00 1224.00

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
