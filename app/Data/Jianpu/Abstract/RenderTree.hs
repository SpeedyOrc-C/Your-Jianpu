module Data.Jianpu.Abstract.RenderTree (engraveMusic) where

import Control.Monad.Reader (asks)
import Data.IntervalMap (Interval (ClosedInterval))
import Data.IntervalMap qualified as IM
import Data.IntervalMap.Lazy (IntervalMap)
import Data.Jianpu.Abstract (Entity (Event, event), Music, Span (Beam))
import Data.Jianpu.Abstract.Error (HasError)
import Data.Jianpu.Graphics (Slice (elements), SliceElement, Slices (..))
import Data.Jianpu.Graphics.Config (RenderConfig (lineGap, lineWidth), RenderContextT, fill, getBeamGap, getBeamHeight, getGlyphWidth)
import Data.Jianpu.Graphics.Render (RenderObject (Rectangle), engraveSliceElement)
import Data.Jianpu.Graphics.Slice (sliceMusic)
import Data.Jianpu.Graphics.Spacing (SpringWithRod (..), computeSpringConstants, sff)
import Data.Jianpu.Types (Event (Action, timeMultiplier))
import Data.Layout (AnchorPosition (APTopLeft), BoundingBox, HasBox (getBox), LayoutTree (..), boxBoundX, boxBoundY, move, moveDown, moveRight)
import Data.List (groupBy, transpose)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Traversable (for)
import Debug.Trace (traceShowM)

type Engraver = RenderContextT HasError (LayoutTree RenderObject)

engraveMusic :: Music -> Engraver
engraveMusic music = do
    lineWidth <- asks lineWidth
    lineGap <- asks lineGap

    let Slices slices spanGroups = sliceMusic music
    let elementsBySlice = elements <$> slices
    let elementsByLine = transpose elementsBySlice

    engravedElementsByLine <- mapM (mapM engraveSliceElement) elementsByLine
    boxesByLine <- mapM (mapM (fill . getBox)) engravedElementsByLine

    let slicesOffsetX = computeSlicesOffsetX slices (transpose boxesByLine) lineWidth

    engravedVoices <- for (zip elementsByLine spanGroups) $ \(elements, spans) -> do
        engravedBaseNotes <- engraveBaseNotes slicesOffsetX elements
        engravedBeams <- engraveBeams slicesOffsetX spans elements
        pure [engravedBaseNotes, engravedBeams]

    (map boxBoundY -> engravedVoicesBoundY) <- traverse (fill . getBox) engravedVoices

    let voicesOffsetsY =
            scanl1 (+) $
                negativeSize (head engravedVoicesBoundY)
                    : zipWith
                        ( \upperLineBottom lowerLineTop ->
                            upperLineBottom + lineGap + lowerLineTop
                        )
                        (positiveSize <$> engravedVoicesBoundY)
                        (negativeSize <$> tail engravedVoicesBoundY)

    traceShowM slicesOffsetX

    pure . LTNode mempty $
        [ LTNode (moveDown offsetY) engravedVoice
        | (engravedVoice, offsetY) <- zip engravedVoices voicesOffsetsY
        ]

engraveBaseNotes :: [Double] -> [SliceElement] -> Engraver
engraveBaseNotes slicesOffsetX elements = do
    engravedBaseElements <- mapM engraveSliceElement elements
    pure . LTNode mempty $
        [ LTNode (moveRight x) [e]
        | (e, x) <- zip engravedBaseElements slicesOffsetX
        ]

computeSlicesOffsetX :: [Slice] -> [[BoundingBox]] -> Double -> [Double]
computeSlicesOffsetX slices slicesBoxes lineWidth =
    init $ scanl1 (+) springExtensions
  where
    springExtensions = [max rodLength (force / springConst) | SWR{..} <- springs]
    force = sff (NE.sort (NE.fromList springs)) lineWidth
    springs = zipWith SWR springMinLengths springConstants
    springConstants = (1 / 0) : computeSpringConstants slices
    springMinLengths =
        maximum (negativeSize <$> head slicesElementsBoundsY)
            : zipWith
                ( \slice1 slice2 ->
                    maximum $
                        zipWith
                            (+)
                            (positiveSize <$> slice1)
                            (negativeSize <$> slice2)
                )
                slicesElementsBoundsY
                (tail slicesElementsBoundsY)
            ++ [maximum (positiveSize <$> last slicesElementsBoundsY)]
    slicesElementsBoundsY = map (map boxBoundX) slicesBoxes

engraveBeams :: [Double] -> IntervalMap Int Span -> [SliceElement] -> Engraver
engraveBeams slicesOffsetX spans voiceElements = do
    beamGap <- asks getBeamGap
    beamHeight <- asks getBeamHeight
    glyphWidth <- asks getGlyphWidth

    pure $ LTNode mempty $ do
        (level, masks) <- zip [1 ..] beamMasksByLevel

        members <-
            -- `groupBy` doesn't filter notes without beams
            -- so remove them after grouping.
            filter (all (\(_, (_, m)) -> m)) $
                groupBy
                    ( \(_, (a, m1)) (_, (b, m2)) ->
                        (m1 && m2) -- Both have beams
                            && inTheSameBeam a b
                    )
                    (zip slicesOffsetX (zip [0 ..] masks))

        let membersOffsetsX = fst <$> members

        let beamRange = case membersOffsetsX of
                [] -> Nothing
                [x] -> Just (x - glyphWidth * 0.55, x + glyphWidth * 0.55)
                x1 : xs -> Just (x1 - glyphWidth * 0.55, last xs + glyphWidth * 0.55)

        case beamRange of
            Nothing -> []
            Just (x1, x2) ->
                [ LTNode
                    (move x1 (beamGap + (level - 1) * (beamHeight + beamGap)))
                    [LTLeaf APTopLeft (Rectangle (x2 - x1) beamHeight)]
                ]
  where
    maxBeamCount = maximum $ beamCount <$> voiceElements

    beamMasksByEntity = do
        element <- voiceElements
        let count = beamCount element
        [replicate count True ++ replicate (maxBeamCount - count) False]

    beamMasksByLevel = transpose beamMasksByEntity

    beamSpans =
        mapMaybe (\case ClosedInterval a b -> Just (a, b); _ -> Nothing) $
            IM.keys $
                IM.mapMaybe (\case Beam -> Just (); _ -> Nothing) spans

    inTheSameBeam a b =
        any (\(spanA, spanB) -> spanA <= a && a <= b && b <= spanB) beamSpans

beamCount :: SliceElement -> Int
beamCount (Just (Right (Event{event = Action{..}}))) = fromEnum timeMultiplier
beamCount _ = 0

negativeSize :: (Double, Double) -> Double
negativeSize (x, _) = max 0 (-x)

positiveSize :: (Double, Double) -> Double
positiveSize (_, x) = max 0 x
