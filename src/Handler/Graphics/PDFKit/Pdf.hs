{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.Graphics.PDFKit.Pdf where

import Import
import Data.Time
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Maybe as M
import qualified Data.ByteString.Char8 as B8
import Handler.Graphics.PDFKit.Helpers

data PdfDocument = PdfDocument
  { pdfDocumentVersion :: Text
  , pdfDocumentHeaderLines :: [ByteString]
  , pdfDocumentCreationDate :: Text
  , pdfDocumentNextObjId :: Int
  , pdfDocumentInfo :: PdfInfo
  , pdfDocumentRoot :: PdfRoot
  , pdfDocumentPages :: PdfPages
  , pdfDocumentStandardFont :: PdfStandardFont
  , pdfDocumentFonts :: [PdfFont]
  , pdfDocumentFontSize :: Int
  , pdfDocumentTrailer :: PdfTrailer
  , pdfDocumentXref :: PdfXref
  , pdfDocumentStartXref :: Maybe Int
  }

instance ToJSON PdfDocument where
  toJSON o = object
    [ "version" .= pdfDocumentVersion o
    , "nextObjId" .= pdfDocumentNextObjId o
    , "info" .= pdfDocumentInfo o
    , "root" .= pdfDocumentRoot o
    , "pages" .= pdfDocumentPages o
    , "standardFont" .= pdfDocumentStandardFont o
    , "fonts" .= pdfDocumentFonts o
    , "fontSize" .= pdfDocumentFontSize o
    , "trailer" .= pdfDocumentTrailer o
    , "xref" .= pdfDocumentXref o
    , "startxref" .= pdfDocumentStartXref o
    ]

instance ToByteStringLines PdfDocument where
  toByteStringLines pdfDoc =
    headerLines
    ++ L.foldl (\acc x -> acc ++ x) [] objectBlocks
    ++ footerLines
    where
      (headerLines, objectBlocks, footerLines) =
        pdfDocumentByteStringLineBlocks pdfDoc

initialPdfDocument :: UTCTime -> TimeZone -> PdfDocument
initialPdfDocument now timeZone =
  PdfDocument
  { pdfDocumentVersion = version
  , pdfDocumentHeaderLines =
    [ encodeUtf8 $ "%PDF-" ++ version
    , "%" ++ B8.pack ['\xff', '\xff', '\xff', '\xff']
    ]
  , pdfDocumentCreationDate = creationDate
  , pdfDocumentNextObjId = nextObjId
  , pdfDocumentInfo =
      PdfInfo
      { pdfInfoObjId = infoObjId
      , pdfInfoProducer = "hs-pdfkit"
      , pdfInfoCreator = "hs-pdfkit"
      , pdfInfoCreationDate = "D:" ++ creationDate ++ "Z"
      }
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
  , pdfDocumentStandardFont = defaultFont
  , pdfDocumentFonts = []
  , pdfDocumentFontSize = defaultFontSize
  , pdfDocumentXref = PdfXref { pdfXrefPositions = [] }
  , pdfDocumentTrailer = PdfTrailer { pdfTrailerSize = Nothing }
  , pdfDocumentStartXref = Nothing
  }
  where
    creationDate = T.pack $ formatLocalTime timeZone now
    defaultFontSize = 24
    version = "1.3"
    infoObjId = 1
    rootObjId = 2
    pagesObjId = 3
    nextObjId = 4

-----------------------------------------------

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
  toByteStringLines pdfInfo =
    [ encodeUtf8 $ (pack $ show $ pdfInfoObjId pdfInfo) ++ " 0 obj"
    , encodeUtf8 $ "% ------------------------------------------------------ info " ++ (intToText $ pdfInfoObjId pdfInfo)
    , encodeUtf8 "<<"
    , encodeUtf8 $ "/Producer (" ++ pdfInfoProducer pdfInfo ++ ")"
    , encodeUtf8 $ "/Creator (" ++ pdfInfoCreator pdfInfo ++ ")"
    , encodeUtf8 $ "/CreationDate (" ++ pdfInfoCreationDate pdfInfo ++ ")"
    , encodeUtf8 ">>"
    , encodeUtf8 "endobj"
    ]

-----------------------------------------------

data PdfRoot = PdfRoot
  { pdfRootObjId :: Int
  , pdfRootPages :: Text
  }

instance ToJSON PdfRoot where
  toJSON o = object
    [ "objId" .= pdfRootObjId o
    , "pages" .= pdfRootPages o
    ]

instance ToByteStringLines (PdfRoot, PdfDocument) where
  toByteStringLines (pdfRoot, pdfDoc) =
    [ encodeUtf8 $ (pack $ show $ pdfRootObjId pdfRoot) ++ " 0 obj"
    , encodeUtf8 $ "% ------------------------------------------------------ root " ++ (intToText $ pdfRootObjId pdfRoot)
    , encodeUtf8 "<<"
    , encodeUtf8 $ "/Type /Catalog"
    , encodeUtf8 $ "/Pages " ++ (ref $ pdfPagesObjId $ pdfDocumentPages pdfDoc)
    , encodeUtf8 ">>"
    , encodeUtf8 "endobj"
    ]

-----------------------------------------------

data PdfPages = PdfPages
  { pdfPagesObjId :: Int
  , pdfPagesKids :: [PdfPage]
  }

instance ToJSON PdfPages where
  toJSON o = object
    [ "objId" .= pdfPagesObjId o
    , "kids" .= pdfPagesKids o
    ]

instance ToByteStringLines PdfPages where
  toByteStringLines pdfPages =
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

-----------------------------------------------

data PdfFont = PdfFont
  { pdfFontObjId :: Int
  , pdfFontName :: Text
  , pdfFontStandardFont ::PdfStandardFont
  }
  deriving Eq

instance ToJSON PdfFont where
  toJSON o = object
    [ "objId" .= pdfFontObjId o
    , "name" .= pdfFontName o
    , "standardFont" .= pdfFontStandardFont o
    ]

instance ToByteStringLines PdfFont where
  toByteStringLines pdfFont =
    [ encodeUtf8 $ (pack $ show $ pdfFontObjId pdfFont) ++ " 0 obj"
    , encodeUtf8 $ "% ------------------------------------------------------ Font " ++ (intToText $ pdfFontObjId pdfFont)
    , encodeUtf8 "<<"
    , encodeUtf8 $ "/Type /Font"
    , encodeUtf8 $ "/Name /" ++ pdfFontName pdfFont
    , encodeUtf8 $ "/BaseFont /" ++ pdfStandardFontBaseFont standardFont
    , encodeUtf8 $ "/Subtype /"  ++ pdfStandardFontSubtype standardFont
    , encodeUtf8 $ "/Encoding /" ++ pdfStandardFontEncoding standardFont
    , encodeUtf8 ">>"
    , encodeUtf8 "endobj"
    ]
    where
      standardFont = pdfFontStandardFont pdfFont

-----------------------------------------------

data PdfResources = PdfResources
  { pdfResourcesObjId :: Int
  , pdfResourcesFonts :: [PdfFont]
  }

instance ToJSON PdfResources where
  toJSON o = object
    [ "objId" .= pdfResourcesObjId o
    , "fonts" .= pdfResourcesFonts o
    ]

instance ToByteStringLines PdfResources where
  toByteStringLines pdfResources =
    [ encodeUtf8 $ (pack $ show $ pdfResourcesObjId pdfResources) ++ " 0 obj"
    , encodeUtf8 $ "% ------------------------------------------------------ Resources " ++ (intToText $ pdfResourcesObjId pdfResources)
    , encodeUtf8 "<<"
    , encodeUtf8 $ "/ProcSet [/PDF /Text /ImageB /ImageC /ImageI]"
    ]
    ++
    ( case pdfResourcesFonts pdfResources of
        [] -> []
        pdfFonts ->
          [ encodeUtf8 $ "/Font <<" ]
          ++
          ( L.map (\pdfFont ->
                     encodeUtf8 $ "/" ++ pdfFontName pdfFont ++ " "
                     ++ (ref $ pdfFontObjId pdfFont)
                  ) pdfFonts
          )
          ++
          [ encodeUtf8 $ ">>" ]
    )
    ++
    [ encodeUtf8 ">>"
    , encodeUtf8 "endobj"
    ]

-----------------------------------------------

data PdfPage = PdfPage
  { pdfPageObjId :: Int
  , pdfPageSize :: PdfPageSize
  , pdfPageMargins :: PdfPageMargins
  , pdfPageLayout :: PdfPageLayout
  , pdfPageResources :: PdfResources
  , pdfPageContents :: PdfContents
  }

instance ToJSON PdfPage where
  toJSON o = object
    [ "objId" .= pdfPageObjId o
    , "size" .= pdfPageSize o
    , "margins" .= pdfPageMargins o
    , "layout" .= ( T.pack $ case pdfPageLayout o of
                               Portrait -> "portrait"
                               Landscape -> "landscape" )
    , "resources" .= pdfPageResources o
    , "contents" .= pdfPageContents o
    ]

instance ToByteStringLines (PdfPage, PdfDocument) where
  toByteStringLines (pdfPage, pdfDoc) =
    [ encodeUtf8 $ (pack $ show $ pdfPageObjId pdfPage) ++ " 0 obj"
    , encodeUtf8 $ "% ------------------------------------------------------ Page " ++ (intToText $ pdfPageObjId pdfPage)
    , encodeUtf8 "<<"
    , encodeUtf8 $ "/Type /Page"
    , encodeUtf8 $ "/Parent " ++ (ref $ pdfPagesObjId $ pdfDocumentPages pdfDoc)
    , encodeUtf8 $ "% " ++ (pack $ show $ pdfPageLayout pdfPage)
    , encodeUtf8 $ "/MediaBox [0 0 "
      ++ (doubleToText $ pdfPageSizeWidth pageSize) ++ " "
      ++ (doubleToText $ pdfPageSizeHeight pageSize) ++ "]"
    , encodeUtf8 $ "/Resources " ++ (ref $ pdfResourcesObjId $ pdfPageResources pdfPage)
    , encodeUtf8 $ "/Contents " ++ (ref $ pdfContentsObjId $ pdfPageContents pdfPage)
    , encodeUtf8 ">>"
    , encodeUtf8 "endobj"
    ]
    where
      pageSize = applyLayout (pdfPageSize pdfPage) (pdfPageLayout pdfPage)

-----------------------------------------------

data PdfContents = PdfContents
  { pdfContentsObjId :: Int
  , pdfContentsTexts :: [PdfText]
  }

instance ToJSON PdfContents where
  toJSON o = object
    [ "objId" .= pdfContentsObjId o
    , "texts" .= pdfContentsTexts o
    ]

instance ToByteStringLines (PdfContents, PdfPage) where
  toByteStringLines (pdfContents, pdfPage) =
    [ encodeUtf8 $ (pack $ show $ pdfContentsObjId pdfContents) ++ " 0 obj"
    , encodeUtf8 $ "% ------------------------------------------------------ Contents " ++ (intToText $ pdfContentsObjId pdfContents)
    , encodeUtf8 "<<"
    , encodeUtf8 $ "/Length " ++ intToText streamLength
    , encodeUtf8 ">>"
    , encodeUtf8 "stream"
    , encodeUtf8 translateOrigin
    ]
    ++
    ( map encodeUtf8 streamTextLines )
    ++
    [ encodeUtf8 "endstream"
    ]
    where
      PdfPageSize {pdfPageSizeHeight = pageHeight} = pdfPageSize pdfPage
      streamTextLines =
        L.foldl
        (\acc pdfText ->
           acc
           ++
           [ "q"
           , translateOrigin
           , "BT"
           , translatePos pdfText
           , "/" ++ ( case pdfTextFont pdfText of
                        Just pdfFont -> pdfFontName pdfFont
                        _ -> ""
                    ) ++ " " ++ (intToText $ pdfTextFontSize pdfText) ++ " Tf"
           , "(" ++ (pdfTextText pdfText) ++ ") Tj"
           , "ET"
           , "Q"
           ]
        ) [] $ pdfContentsTexts pdfContents
      translateOrigin = "1 0 0 -1 0 " ++ (doubleToText pageHeight) ++ " cm"
      translatePos pdfText =
        "1 0 0 1 " ++ (doubleToText $ pdfTextX pdfText)
        ++ " " ++ (doubleToText $ pageHeight-(pdfTextY pdfText)) ++ " Tm"
      streamLength :: Int
      streamLength = length $ encodeUtf8 $ unlines streamTextLines

-----------------------------------------------

data PdfText = PdfText
  { pdfTextText :: Text
  , pdfTextX :: Double
  , pdfTextY :: Double
  , pdfTextFont :: Maybe PdfFont
  , pdfTextFontSize :: Int
  }

instance ToJSON PdfText where
  toJSON o = object
    [ "text" .= pdfTextText o
    , "x" .= pdfTextX o
    , "y" .= pdfTextY o
    , "font" .= pdfTextFont o
    , "fontSize" .= pdfTextFontSize o
    ]

-----------------------------------------------

data PdfXref = PdfXref
  { pdfXrefPositions :: [Int]
  }

instance ToJSON PdfXref where
  toJSON o = object
    [ "positions" .= pdfXrefPositions o
    ]

instance ToByteStringLines (PdfXref, PdfDocument) where
  toByteStringLines (pdfXref, pdfDoc) =
    [ encodeUtf8 "xref"
    , encodeUtf8 "% ------------------------------------------------------ xref"
    , encodeUtf8 $ "0 " ++ ( case pdfTrailerSize $ pdfDocumentTrailer pdfDoc of
                               Just size -> intToText $ 1 + size
                               _ -> ""
                           )
    , encodeUtf8 $ "0000000000 65535 f"
    ]
    ++
    (L.map (\pos -> encodeUtf8 $ (formatXrefPos pos) ++ " 00000 n") $ pdfXrefPositions pdfXref)

-----------------------------------------------

data PdfTrailer = PdfTrailer
  { pdfTrailerSize :: Maybe Int
  }

instance ToJSON PdfTrailer where
  toJSON o = object
    [ "size" .= pdfTrailerSize o
    ]

instance ToByteStringLines (PdfTrailer, PdfDocument) where
  toByteStringLines (pdfTrailer, pdfDoc) =
    [ encodeUtf8 "trailer"
    , encodeUtf8 "% ------------------------------------------------------ trailer"
    , encodeUtf8 "<<"
    , encodeUtf8 $ "/Size " ++ (maybeIntToText $ pdfTrailerSize pdfTrailer)
    , encodeUtf8 $ "/Root " ++ (ref $ pdfRootObjId $ pdfDocumentRoot pdfDoc)
    , encodeUtf8 $ "/Info " ++ (ref $ pdfInfoObjId $ pdfDocumentInfo pdfDoc)
    , encodeUtf8 ">>"
    ]

-----------------------------------------------

data PdfPageMargins =
  PdfPageMargins
  { pdfPageMarginTop :: Double
  , pdfPageMarginLeft :: Double
  , pdfPageMarginBottom :: Double
  , pdfPageMarginRight :: Double
  }

instance ToJSON PdfPageMargins where
  toJSON o = object
    [ "top" .= pdfPageMarginTop o
    , "left" .= pdfPageMarginLeft o
    , "bottom" .= pdfPageMarginBottom o
    , "right" .= pdfPageMarginRight o
    ]

defaultPageMargins :: PdfPageMargins
defaultPageMargins = PdfPageMargins 72 72 72 72

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

defaultPageSize :: PdfPageSize
defaultPageSize = sA4

s4A0 :: PdfPageSize
s4A0 = PdfPageSize 4767.87 6740.79
s2A0 :: PdfPageSize
s2A0 = PdfPageSize 3370.39 4767.87
sA0 :: PdfPageSize
sA0 = PdfPageSize 2383.94 3370.39
sA1 :: PdfPageSize
sA1 = PdfPageSize 1683.78 2383.94
sA2 :: PdfPageSize
sA2 = PdfPageSize 1190.55 1683.78
sA3 :: PdfPageSize
sA3 = PdfPageSize 841.89 1190.55
sA4 :: PdfPageSize
sA4 = PdfPageSize 595.28 841.89
sA5 :: PdfPageSize
sA5 = PdfPageSize 419.53 595.28
sA6 :: PdfPageSize
sA6 = PdfPageSize 297.64 419.53
sA7 :: PdfPageSize
sA7 = PdfPageSize 209.76 297.64
sA8 :: PdfPageSize
sA8 = PdfPageSize 147.40 209.76
sA9 :: PdfPageSize
sA9 = PdfPageSize 104.88 147.40
sA10 :: PdfPageSize
sA10 = PdfPageSize 73.70 104.88
sB0 :: PdfPageSize
sB0 = PdfPageSize 2834.65 4008.19
sB1 :: PdfPageSize
sB1 = PdfPageSize 2004.09 2834.65
sB2 :: PdfPageSize
sB2 = PdfPageSize 1417.32 2004.09
sB3 :: PdfPageSize
sB3 = PdfPageSize 1000.63 1417.32
sB4 :: PdfPageSize
sB4 = PdfPageSize 708.66 1000.63
sB5 :: PdfPageSize
sB5 = PdfPageSize 498.90 708.66
sB6 :: PdfPageSize
sB6 = PdfPageSize 354.33 498.90
sB7 :: PdfPageSize
sB7 = PdfPageSize 249.45 354.33
sB8 :: PdfPageSize
sB8 = PdfPageSize 175.75 249.45
sB9 :: PdfPageSize
sB9 = PdfPageSize 124.72 175.75
sB10 :: PdfPageSize
sB10 = PdfPageSize 87.87 124.72
sC0 :: PdfPageSize
sC0 = PdfPageSize 2599.37 3676.54
sC1 :: PdfPageSize
sC1 = PdfPageSize 1836.85 2599.37
sC2 :: PdfPageSize
sC2 = PdfPageSize 1298.27 1836.85
sC3 :: PdfPageSize
sC3 = PdfPageSize 918.43 1298.27
sC4 :: PdfPageSize
sC4 = PdfPageSize 649.13 918.43
sC5 :: PdfPageSize
sC5 = PdfPageSize 459.21 649.13
sC6 :: PdfPageSize
sC6 = PdfPageSize 323.15 459.21
sC7 :: PdfPageSize
sC7 = PdfPageSize 229.61 323.15
sC8 :: PdfPageSize
sC8 = PdfPageSize 161.57 229.61
sC9 :: PdfPageSize
sC9 = PdfPageSize 113.39 161.57
sC10 :: PdfPageSize
sC10 = PdfPageSize 79.37 113.39
sRA0 :: PdfPageSize
sRA0 = PdfPageSize 2437.80 3458.27
sRA1 :: PdfPageSize
sRA1 = PdfPageSize 1729.13 2437.80
sRA2 :: PdfPageSize
sRA2 = PdfPageSize 1218.90 1729.13
sRA3 :: PdfPageSize
sRA3 = PdfPageSize 864.57 1218.90
sRA4 :: PdfPageSize
sRA4 = PdfPageSize 609.45 864.57
sSRA0 :: PdfPageSize
sSRA0 = PdfPageSize 2551.18 3628.35
sSRA1 :: PdfPageSize
sSRA1 = PdfPageSize 1814.17 2551.18
sSRA2 :: PdfPageSize
sSRA2 = PdfPageSize 1275.59 1814.17
sSRA3 :: PdfPageSize
sSRA3 = PdfPageSize 907.09 1275.59
sSRA4 :: PdfPageSize
sSRA4 = PdfPageSize 637.80 907.09
sExecutive :: PdfPageSize
sExecutive = PdfPageSize 521.86 756.00
sFolio :: PdfPageSize
sFolio = PdfPageSize 612.00 936.00
sLegal :: PdfPageSize
sLegal = PdfPageSize 612.00 1008.00
sLetter :: PdfPageSize
sLetter = PdfPageSize 612.00 792.00
sTabloid :: PdfPageSize
sTabloid = PdfPageSize 792.00 1224.00

-----------------------------------------------

data PdfStandardFont = PdfStandardFont
  { pdfStandardFontBaseFont :: Text
  , pdfStandardFontSubtype :: Text
  , pdfStandardFontEncoding :: Text
  }
  deriving Eq

instance ToJSON PdfStandardFont where
  toJSON o = object
    [ "baseFont" .= pdfStandardFontBaseFont o
    , "subtype" .= pdfStandardFontSubtype o
    , "encoding" .= pdfStandardFontEncoding o
    ]

defaultFont :: PdfStandardFont
defaultFont = helvetica

courier :: PdfStandardFont
courier = PdfStandardFont "Courier" "Type1" "WinAnsiEncoding"
courierBold :: PdfStandardFont
courierBold = PdfStandardFont "Courier-Bold" "Type1" "WinAnsiEncoding"
courierOblique :: PdfStandardFont
courierOblique = PdfStandardFont "Courier-Oblique" "Type1" "WinAnsiEncoding"
courierBoldOblique :: PdfStandardFont
courierBoldOblique = PdfStandardFont "Courier-BoldOblique" "Type1" "WinAnsiEncoding"
helvetica :: PdfStandardFont
helvetica = PdfStandardFont "Helvetica" "Type1" "WinAnsiEncoding"
helveticaBold :: PdfStandardFont
helveticaBold = PdfStandardFont "Helvetica-Bold" "Type1" "WinAnsiEncoding"
helveticaOblique :: PdfStandardFont
helveticaOblique = PdfStandardFont "Helvetica-Oblique" "Type1" "WinAnsiEncoding"
helveticaBoldOblique :: PdfStandardFont
helveticaBoldOblique = PdfStandardFont "Helvetica-BoldOblique" "Type1" "WinAnsiEncoding"
timesRoman :: PdfStandardFont
timesRoman = PdfStandardFont "Times-Roman" "Type1" "WinAnsiEncoding"
timesBold :: PdfStandardFont
timesBold = PdfStandardFont "Times-Bold" "Type1" "WinAnsiEncoding"
timesItalic :: PdfStandardFont
timesItalic = PdfStandardFont "Times-Italic" "Type1" "WinAnsiEncoding"
timesBoldItalic :: PdfStandardFont
timesBoldItalic = PdfStandardFont "Times-BoldItalic" "Type1" "WinAnsiEncoding"
symbol :: PdfStandardFont
symbol = PdfStandardFont "Symbol" "Type1" "WinAnsiEncoding"
zapfDingbats :: PdfStandardFont
zapfDingbats = PdfStandardFont "ZapfDingbats" "Type1" "WinAnsiEncoding"

currentFont :: PdfDocument -> Maybe PdfFont
currentFont pdfDoc =
  L.find
  (\pdfFont -> pdfFontStandardFont pdfFont == pdfDocumentStandardFont pdfDoc) $
  pdfDocumentFonts pdfDoc

currentFontId :: PdfDocument -> Maybe Int
currentFontId pdfDoc =
  case currentFont pdfDoc of
    Just pdfFont -> Just $ pdfFontObjId pdfFont
    _ -> Nothing

-----------------------------------------------

class ToByteStringLines b where
  toByteStringLines :: b -> [ByteString]

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
  ( -- header lines
    pdfDocumentHeaderLines pdfDoc
  , -- referencable line-blocks
    [ toByteStringLines $ pdfDocumentInfo pdfDoc
    , toByteStringLines (pdfDocumentRoot pdfDoc, pdfDoc)
    , toByteStringLines $ pdfDocumentPages pdfDoc
    ]
    ++
    ( L.foldl
      (\acc pdfPage ->
          [ toByteStringLines (pdfPage, pdfDoc)
          , toByteStringLines $ pdfPageResources pdfPage
          , toByteStringLines (pdfPageContents pdfPage, pdfPage)
          ]
          ++ acc
      )
      []
      (pdfPagesKids $ pdfDocumentPages pdfDoc)
    )
    ++
    ( L.map (\pdfFont -> toByteStringLines pdfFont) $ pdfDocumentFonts pdfDoc )
  , -- footer lines
    ( toByteStringLines (pdfDocumentXref pdfDoc, pdfDoc))
    ++ ( toByteStringLines (pdfDocumentTrailer pdfDoc, pdfDoc))
    ++ [ encodeUtf8 "startxref"
       , encodeUtf8 $ maybeIntToText $ pdfDocumentStartXref pdfDoc
       , encodeUtf8 "%%EOF"
       ]
  )

-----------------------------------------------

data Action =
  ActionInfoSetProducer Text
  | ActionInfoSetCreator Text
  | ActionFinalize
  | ActionFont PdfStandardFont
  | ActionFontAddIfMissing
  | ActionFontSetSize Int
  | ActionPage
  | ActionPageSetSize PdfPageSize
  | ActionPageSetLayout PdfPageLayout
  | ActionPageSetMargin Double
  | ActionPageSetMargins Double Double Double Double
  | ActionPageSetSizeCustom Double Double
  | ActionText Text Double Double

build :: Action -> PdfBuilder
build action = PdfBuilderM () [action]

-----------------------------------------------

instance IsExecutableAction Action where

  execute (ActionInfoSetProducer text) pdfDoc =
    pdfDoc
    { pdfDocumentInfo =
        (pdfDocumentInfo pdfDoc) { pdfInfoProducer = text}
    }

  execute (ActionInfoSetCreator text) pdfDoc =
    pdfDoc
    { pdfDocumentInfo =
        (pdfDocumentInfo pdfDoc) { pdfInfoCreator = text}
    }

  execute ActionFinalize pdfDoc =
    pdfDoc
    { pdfDocumentXref = PdfXref { pdfXrefPositions = L.init positions }
    , pdfDocumentStartXref = Just $ L.last positions
    , pdfDocumentTrailer =
        (pdfDocumentTrailer pdfDoc) { pdfTrailerSize = Just $ pdfDocumentNextObjId pdfDoc - 1 }
    }
    where
      (headerLines, objectBlocks, _) = pdfDocumentByteStringLineBlocks pdfDoc
      headerLength = length $ B8.unlines headerLines
      objectLines = L.map B8.unlines objectBlocks
      lengths = L.map length objectLines
      (positions, _) = L.foldl (\(ls,accl) len -> (ls ++ [accl+len], accl+len)) ([headerLength], headerLength) lengths

  execute (ActionFont standardFont) pdfDoc =
    pdfDoc { pdfDocumentStandardFont = standardFont }

  execute (ActionFontSetSize fontSize) pdfDoc =
    pdfDoc { pdfDocumentFontSize = fontSize }

  execute (ActionFontAddIfMissing) pdfDoc =
    pdfDoc
    { pdfDocumentNextObjId = nextObjId
    , pdfDocumentFonts =
        (pdfDocumentFonts pdfDoc)
        ++
        if fontAlreadyAdded
        then []
        else [ PdfFont
               { pdfFontObjId = fontObjId
               , pdfFontName = "F" ++ intToText fontObjId
               , pdfFontStandardFont = pdfDocumentStandardFont pdfDoc
               }
             ]
    }
    where
      fontAlreadyAdded = M.isJust $ currentFont pdfDoc
      fontObjId = pdfDocumentNextObjId pdfDoc
      nextObjId = if fontAlreadyAdded
                  then pdfDocumentNextObjId pdfDoc
                  else pdfDocumentNextObjId pdfDoc + 1+ 1

  execute ActionPage pdfDoc =
    pdfDoc
    { pdfDocumentNextObjId = nextObjId
    , pdfDocumentPages =
        (pdfDocumentPages pdfDoc)
        { pdfPagesKids =
            (pdfPagesKids $ pdfDocumentPages pdfDoc)
            ++
            [ PdfPage
              { pdfPageObjId = pageObjId
              , pdfPageSize = defaultPageSize
              , pdfPageMargins = defaultPageMargins
              , pdfPageLayout = Portrait
              , pdfPageResources =
                  PdfResources
                  { pdfResourcesObjId = resourcesObjId
                  , pdfResourcesFonts = []
                  }
              , pdfPageContents =
                  PdfContents
                  { pdfContentsObjId = contentsObjId
                  , pdfContentsTexts = []
                  }
              }
            ]
        }
    }
    where
      pageObjId = pdfDocumentNextObjId pdfDoc
      resourcesObjId = pdfDocumentNextObjId pdfDoc + 1
      contentsObjId = pdfDocumentNextObjId pdfDoc + 2
      nextObjId = pdfDocumentNextObjId pdfDoc + 3

  execute (ActionPageSetSize size) pdfDoc =
    pdfDoc
    { pdfDocumentPages =
        pdfPages
        { pdfPagesKids =
            initPages
            ++
            [ lastPage { pdfPageSize = size } ]
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
          [ lastPage { pdfPageLayout = lay } ]
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
          [ lastPage { pdfPageMargins = PdfPageMargins x x x x } ]
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
          [ lastPage { pdfPageMargins = PdfPageMargins top left bottom right } ]
        }
    }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc

  execute (ActionText t x y) pdfDoc =
    pdfDoc
    { pdfDocumentPages =
        pdfPages
        { pdfPagesKids =
          initPages
          ++
          [ lastPage
            { pdfPageResources =
                (pdfPageResources lastPage)
                { pdfResourcesFonts =
                    L.nub $ ( pageFonts lastPage
                              ++
                              case currentFont pdfDoc of
                                Just pdfFont -> [pdfFont]
                                _ -> []
                            )
                }
            , pdfPageContents =
                (pdfPageContents lastPage)
                { pdfContentsTexts =
                    (pdfContentsTexts $ pdfPageContents lastPage)
                    ++
                    [ PdfText
                      { pdfTextText = t
                      , pdfTextX = x
                      , pdfTextY = y
                      , pdfTextFont = currentFont pdfDoc
                      , pdfTextFontSize = pdfDocumentFontSize pdfDoc
                      }
                  ]
                }
            }
          ]
        }
    }
    where
      (pdfPages, initPages, lastPage) = pdfPagesTuple pdfDoc
      pageFonts = pdfResourcesFonts . pdfPageResources
