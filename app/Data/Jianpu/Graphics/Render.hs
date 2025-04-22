module Data.Jianpu.Graphics.Render where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Jianpu.Abstract
import Data.Jianpu.Abstract.Error (HasError)
import Data.Jianpu.Graphics
import Data.Jianpu.Graphics.Config
import Data.Jianpu.Types
import Data.Layout
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Debug.Trace (trace, traceM, traceShow, traceShowId, traceShowM)

data AlignText = ATLeft | ATRight | ATCentre
    deriving (Show, Eq)

data RenderObject
    = Glyph Glyph
    | GAccidental GAccidental
    | Circle Double
    | Rectangle Double Double
    | Curve Double Double
    | Text Double AlignText String
    | InvisibleRectangle Double Double
    deriving (Show)

data Glyph = GX | G0 | G1 | G2 | G3 | G4 | G5 | G6 | G7
    deriving (Show)

data GAccidental = GNatural | GSharp | GFlat | GDoubleSharp | GDoubleFlat
    deriving (Show)

instance HasSize RenderConfig RenderObject where
    getSize :: RenderObject -> RenderContext Size
    getSize Glyph{} = do
        glyphHeight <- asks getGlyphHeight
        glyphWidth <- asks getGlyphWidth
        pure (glyphWidth, glyphHeight)
    getSize GAccidental{} = do
        accidentalHeight <- asks getAccidentalHeight
        accidentalWidth <- asks getAccidentalWidth
        pure (accidentalWidth, accidentalHeight)
    getSize (Circle r) = pure (2 * r, 2 * r)
    getSize (Rectangle sx sy) = pure (sx, sy)
    getSize (InvisibleRectangle sx sy) = pure (sx, sy)
    getSize (Text size _ _) = pure (0, size)

(<+>) :: (Applicative f) => f a -> f b -> f (a, b)
fa <+> fb = (,) <$> fa <*> fb

{-
TODO:
Need more information about vertical spacing of other notes
when there are ties inside chords.
Add them as new parameters in the future!
-}
engraveSliceElement :: SliceElement -> RenderContextT HasError (LayoutTree RenderObject)
engraveSliceElement Nothing = pure $ LTNode mempty []
engraveSliceElement (Just (Left _)) = pure $ LTNode mempty []
engraveSliceElement (Just (Right (Event{event}))) = drawEvent event
engraveSliceElement (Just (Right (Tag tag))) = drawTag tag

drawTag :: Tag -> RenderContextT HasError (LayoutTree RenderObject)
drawTag BarLine = do
    barLineLength <- asks getBarLineLength
    barLineWidth <- asks getBarLineWidth
    barLineLeftPadding <- asks getBarLineLeftPadding
    barLineRightPadding <- asks getBarLineRightPadding
    glyphHeight <- asks getGlyphHeight

    pure $
        LTNode
            (moveUp (glyphHeight / 2))
            [ LTLeaf APCentre (Rectangle barLineWidth barLineLength)
            , LTLeaf APRight (InvisibleRectangle barLineLeftPadding barLineLength)
            , LTLeaf APLeft (InvisibleRectangle barLineRightPadding barLineLength)
            ]
drawTag EndSign = do
    barLineLength <- asks getBarLineLength
    barLineWidth <- asks getBarLineWidth
    barLineLeftPadding <- asks getBarLineLeftPadding
    barLineRightPadding <- asks getBarLineRightPadding
    thickBarLineWidth <- asks getThickBarLineWidth
    thickBarLineGap <- asks getThickBarLineGap
    glyphHeight <- asks getGlyphHeight

    pure $
        LTNode
            (moveUp (glyphHeight / 2))
            [ LTNode
                (moveLeft thickBarLineGap)
                [LTLeaf APCentre (Rectangle barLineWidth barLineLength)]
            , LTLeaf APLeft (Rectangle thickBarLineWidth barLineLength)
            , LTLeaf APRight (InvisibleRectangle barLineLeftPadding barLineLength)
            , LTLeaf APLeft (InvisibleRectangle barLineRightPadding barLineLength)
            ]
drawTag TimeSignature{} = pure $ LTNode mempty []

drawEvent :: Event -> RenderContextT HasError (LayoutTree RenderObject)
drawEvent Action{..} = drawSound sound dot timeMultiplier
drawEvent Repeater4 = do
    glyphHeight <- asks getGlyphHeight
    repeater4Height <- asks getRepeater4Height
    repeater4Width <- asks getRepeater4Width

    pure $
        LTNode
            (moveUp (glyphHeight / 2))
            [LTLeaf APCentre (Rectangle repeater4Width repeater4Height)]
drawEvent (Pronounce Nothing) = pure $ LTNode mempty []
drawEvent (Pronounce (Just Syllable{..})) = do
    glyphHeight <- asks getGlyphHeight
    pure $ LTLeaf APBottom (Text glyphHeight ATCentre content)

drawSound ::
    Sound ->
    Int ->
    TimeMultiplier ->
    RenderContextT HasError (LayoutTree RenderObject)
drawSound Rest dot _ = do
    dotRadius <- asks getDotRadius
    dotGap <- asks getDotRadius
    glyphHeight <- asks getGlyphHeight
    glyphWidth <- asks getGlyphWidth

    pure . LTNode mempty $
        [ LTLeaf APBottom (Glyph G0)
        , -- Dots to the right
          LTNode
            (move (glyphWidth / 2 + dotGap) (-(glyphHeight / 2)))
            [ LTNode
                (moveRight $ (i - 1) * (dotGap + 2 * dotRadius))
                [LTLeaf APLeft (Circle dotRadius)]
            | i <- fromIntegral <$> [1 .. dot]
            ]
        ]
drawSound Clap dot _ = do
    dotRadius <- asks getDotRadius
    dotGap <- asks getDotRadius
    glyphHeight <- asks getGlyphHeight
    glyphWidth <- asks getGlyphWidth

    pure . LTNode mempty $
        [ LTLeaf APBottom (Glyph GX)
        , -- Dots to the right
          LTNode
            (move (glyphWidth / 2 + dotGap) (-(glyphHeight / 2)))
            [ LTNode
                (moveRight $ (i - 1) * (dotGap + 2 * dotRadius))
                [LTLeaf APLeft (Circle dotRadius)]
            | i <- fromIntegral <$> [1 .. dot]
            ]
        ]
drawSound (Note{pitches = Pitch{..} :| []}) dot timeMultiplier = do
    beamHeight <- asks getBeamHeight
    beamGap <- asks getBeamGap
    transposeDotGap <- asks getTransposeDotGap
    transposeDotRadius <- asks getTransposeDotRadius
    dotRadius <- asks getDotRadius
    dotGap <- asks getDotRadius
    glyphHeight <- asks getGlyphHeight
    glyphWidth <- asks getGlyphWidth

    pure . LTNode mempty $
        -- Transpose dots above
        [ LTNode
            (moveUp $ glyphHeight + transposeDotGap)
            [ LTNode
                (moveUp $ (i - 1) * (transposeDotGap + 2 * transposeDotRadius))
                [LTLeaf APBottom (Circle transposeDotRadius)]
            | i <- fromIntegral <$> [1 .. octaveTranspose]
            ]
        , -- Accidental
          LTNode
            (move (-(glyphWidth / 2)) (-glyphHeight))
            (maybeToList (LTLeaf APTopRight . drawAccidental <$> accidental))
        , -- The note
          LTLeaf
            APBottom
            (drawWhiteKey whiteKey)
        , -- Dots to the right
          LTNode
            (move (glyphWidth / 2 + dotGap) (-(glyphHeight / 2)))
            [ LTNode
                (moveRight $ (i - 1) * (dotGap + 2 * dotRadius))
                [LTLeaf APLeft (Circle dotRadius)]
            | i <- fromIntegral <$> [1 .. dot]
            ]
        , -- Reserve space for beams...
          LTNode
            (moveDown $ (beamGap + beamHeight) * fromIntegral (fromEnum timeMultiplier) + transposeDotGap)
            -- ... and then dots above
            [ LTNode
                (moveDown $ (i - 1) * (transposeDotGap + 2 * transposeDotRadius))
                [LTLeaf APTop (Circle transposeDotRadius)]
            | i <- fromIntegral <$> [1 .. (-octaveTranspose)]
            ]
        ]

drawAccidental :: Accidental -> RenderObject
drawAccidental a = GAccidental $ case a of
    Natural -> GNatural
    Sharp -> GSharp
    Flat -> GFlat
    DoubleSharp -> GDoubleSharp
    DoubleFlat -> GDoubleFlat

drawWhiteKey :: WhiteKey -> RenderObject
drawWhiteKey k = Glyph $ case k of
    K1 -> G1
    K2 -> G2
    K3 -> G3
    K4 -> G4
    K5 -> G5
    K6 -> G6
    K7 -> G7
