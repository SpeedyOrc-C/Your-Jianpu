{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Data.Jianpu.Graphics.Config where

import Control.Monad.Reader (Reader, ReaderT, MonadReader (reader), runReader)
import Data.Jianpu.Types (TimeSignature)
import Data.Jianpu.Abstract.Error (HasError)

type RenderContext = Reader RenderConfig
type RenderContextT m = ReaderT RenderConfig m
type TryRenderContext = RenderContextT HasError

fill :: Monad m => Reader r a -> ReaderT r m a
fill = reader . runReader

{- |
Names with an apostrophe are ratios.
e.g. abc'def has the value abc / def.
-}
data RenderConfig = RCfg
    { -- Graphical config
      pageHeight :: Double
    , lineWidth :: Double
    , lineGap :: Double
    , glyphHeight'lineWidth :: Double
    , glyphWidth'glyphHeight :: Double
    , repeater4Height'glyphHeight :: Double
    , repeater4Width'glyphWidth :: Double
    , transposeDotRadius'glyphHeight :: Double
    , transposeDotGap'glyphHeight :: Double
    , dotRadius'glyphHeight :: Double
    , dotGap'glyphHeight :: Double
    , beamHeight'glyphHeight :: Double
    , beamGap'glyphHeight :: Double
    , accidentalHeight'glyphHeight :: Double
    , accidentalWidth'accidentalHeight :: Double
    , barLineLength'glyphHeight :: Double
    , barLineWidth'glyphWidth :: Double
    , barLineLeftPadding'glyphWidth :: Double
    , barLineRightPadding'glyphWidth :: Double
    , thickBarLineWidth'barLineWidth :: Double
    , thickBarLineGap'barLineWidth :: Double
    , slurHeight'glyphHeight :: Double
    , slurPaddingX'glyphWidth :: Double
    , slurPaddingBottom'glyphHeight :: Double
    , -- Logical config
      initialTimeSignature :: TimeSignature
    }

getGlyphHeight RCfg{..} = glyphHeight'lineWidth * lineWidth
getGlyphWidth cfg@RCfg{..} = glyphWidth'glyphHeight * getGlyphHeight cfg
getRepeater4Height cfg@RCfg{..} = repeater4Height'glyphHeight * getGlyphHeight cfg
getRepeater4Width cfg@RCfg{..} = repeater4Width'glyphWidth * getGlyphWidth cfg
getTransposeDotRadius cfg@RCfg{..} = transposeDotRadius'glyphHeight * getGlyphHeight cfg
getTransposeDotGap cfg@RCfg{..} = transposeDotGap'glyphHeight * getGlyphHeight cfg
getDotRadius cfg@RCfg{..} = dotRadius'glyphHeight * getGlyphHeight cfg
getDotGap cfg@RCfg{..} = dotGap'glyphHeight * getGlyphHeight cfg
getBeamHeight cfg@RCfg{..} = beamHeight'glyphHeight * getGlyphHeight cfg
getBeamGap cfg@RCfg{..} = beamGap'glyphHeight * getGlyphHeight cfg
getAccidentalHeight cfg@RCfg{..} = accidentalHeight'glyphHeight * getGlyphHeight cfg
getAccidentalWidth cfg@RCfg{..} = accidentalWidth'accidentalHeight * getAccidentalHeight cfg
getBarLineLength cfg@RCfg{..} = barLineLength'glyphHeight * getGlyphHeight cfg
getBarLineWidth cfg@RCfg{..} = barLineWidth'glyphWidth * getGlyphWidth cfg
getBarLineLeftPadding cfg@RCfg{..} = barLineLeftPadding'glyphWidth * getGlyphWidth cfg
getBarLineRightPadding cfg@RCfg{..} = barLineRightPadding'glyphWidth * getGlyphWidth cfg
getThickBarLineWidth cfg@RCfg{..} = thickBarLineWidth'barLineWidth * getBarLineWidth cfg
getThickBarLineGap cfg@RCfg{..} = thickBarLineGap'barLineWidth * getBarLineWidth cfg
getSlurHeight cfg@RCfg{..} = slurHeight'glyphHeight * getGlyphHeight cfg
getSlurPaddingX cfg@RCfg{..} = slurPaddingX'glyphWidth * getGlyphWidth cfg
getSlurPaddingBottom cfg@RCfg{..} = slurPaddingBottom'glyphHeight * getGlyphHeight cfg

defaultRenderConfig :: RenderConfig
defaultRenderConfig =
    RCfg
        { pageHeight = 2828.4
        , lineWidth = 1500
        , lineGap = 30
        , glyphHeight'lineWidth = 1 / 50
        , glyphWidth'glyphHeight = 3 / 4
        , repeater4Height'glyphHeight = 1 / 8
        , repeater4Width'glyphWidth = 9 / 10
        , transposeDotRadius'glyphHeight = 1 / 8
        , transposeDotGap'glyphHeight = 1 / 8
        , dotRadius'glyphHeight = 1 / 8
        , dotGap'glyphHeight = 1 / 8
        , beamHeight'glyphHeight = 1 / 10
        , beamGap'glyphHeight = 1 / 8
        , accidentalHeight'glyphHeight = 7 / 10
        , accidentalWidth'accidentalHeight = 1 / 2
        , barLineLength'glyphHeight = 3 / 2
        , barLineWidth'glyphWidth = 1 / 10
        , barLineLeftPadding'glyphWidth = 1
        , barLineRightPadding'glyphWidth = 1
        , thickBarLineWidth'barLineWidth = 3
        , thickBarLineGap'barLineWidth = 2
        , slurHeight'glyphHeight = 1 / 2
        , slurPaddingX'glyphWidth = 1 / 8
        , slurPaddingBottom'glyphHeight = 1 / 8
        , initialTimeSignature = (4, 4)
        }
