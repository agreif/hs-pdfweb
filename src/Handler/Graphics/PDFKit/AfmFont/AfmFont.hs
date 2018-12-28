module Handler.Graphics.PDFKit.AfmFont.AfmFont where

import Import

data AfmFont = AfmFont
  { afmFontFontName :: Text
  , afmFontFullName :: Text
  , afmFontFamilyName :: Text
  , afmFontWeight :: Text
  , afmFontItalicAngle :: Double
  , afmFontIsFixedPitch :: Bool
  , afmFontCharacterSet :: Text
  , afmFontFontBBox :: (Double, Double, Double, Double)
  , afmFontUnderlinePosition :: Double
  , afmFontUnderlineThickness :: Double
  , afmFontVersion :: Text
  , afmFontEncodingScheme :: Text
  , afmFontCapHeight :: Maybe Double
  , afmFontXHeight :: Maybe Double
  , afmFontAscender :: Maybe Double
  , afmFontDescender :: Maybe Double
  , afmFontStdHW :: Double
  , afmFontStdVW :: Double
  , afmFontCharMetrics :: [AfmCharMetric]
  , afmFontKernPairXs :: [AfmKernPairX]
  }
  deriving Eq

data AfmCharMetric = AfmCharMetric
  { afmCharMetricCharCode :: Double
  , afmCharMetricWidthX :: Double
  , afmCharMetricCharName :: Text
  , afmCharMetricBBox :: (Double, Double, Double, Double)
  , afmCharMetricLigatures :: [(Text, Text)]
  }
  deriving Eq

data AfmKernPairX = AfmKernPairX
  { afmKernPairXChar1Name :: Text
  , afmKernPairXChar2Name :: Text
  , afmKernPairXAmount :: Double
  }
  deriving Eq
