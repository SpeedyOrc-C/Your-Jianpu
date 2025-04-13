{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Data.Jianpu.Graphics.Config where

{-|
Names with an apostrophe are ratios.
e.g. abc'def has the value abc / def.
-}
data RenderConfig = RCfg
    { pageHeight :: Double
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

defaultRenderConfig :: RenderConfig
defaultRenderConfig =
    RCfg
        { pageHeight = 1414.2
        , lineWidth = 1000
        , lineGap = 50
        , glyphHeight'lineWidth = 1 / 20
        , glyphWidth'glyphHeight = 3 / 4
        , repeater4Height'glyphHeight = 1 / 8
        , repeater4Width'glyphWidth = 9 / 10
        , transposeDotRadius'glyphHeight = 1 / 8
        , transposeDotGap'glyphHeight = 1 / 8
        , dotRadius'glyphHeight = 1 / 8
        , dotGap'glyphHeight = 1 / 8
        , beamHeight'glyphHeight = 1 / 10
        , beamGap'glyphHeight = 1 / 10
        , accidentalHeight'glyphHeight = 7 / 10
        , accidentalWidth'accidentalHeight = 1 / 2
        }
