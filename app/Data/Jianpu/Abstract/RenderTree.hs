module Data.Jianpu.Abstract.RenderTree (engraveMusic) where

import Control.Monad.Reader (MonadReader, MonadTrans (lift), asks)
import Control.Monad.State.Strict (MonadState (get, put), StateT, evalStateT)
import Data.Array (listArray, (!))
import Data.Function (on)
import Data.IntervalMap.Generic.Strict (IntervalMap)
import Data.IntervalMap.Generic.Strict qualified as IM
import Data.Jianpu.Abstract (Entity (Event, event), Interval (I), Music, Span (Beam, Slur, Tie), SpanWithRenderOrder (SRO), Spans)
import Data.Jianpu.Graphics (Slice (elements), SliceElement, Slices (..))
import Data.Jianpu.Graphics.Config (RenderConfig (lineGap, lineWidth, slurPaddingBottom'glyphHeight, slurPaddingX'glyphWidth), TryRenderContext, fill, getBeamGap, getBeamHeight, getGlyphWidth, getSlurHeight, getSlurPaddingBottom, getSlurPaddingX)
import Data.Jianpu.Graphics.Render (RenderObject (Curve, Rectangle), engraveSliceElement)
import Data.Jianpu.Graphics.Slice (sliceMusic)
import Data.Jianpu.Graphics.Spacing (SpringWithRod (..), computeSpringConstants, sff)
import Data.Jianpu.Types (Event (Action, timeMultiplier))
import Data.Layout (AnchorPosition (APBottom, APBottomLeft, APTopLeft), BoundingBox (BBox, NoBox), HasBox (getBox), LayoutTree (..), boxBound, boxBoundX, boxBoundY, move, moveDown, moveRight)
import Data.List (groupBy, sortBy, transpose)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Traversable (for)
import Debug.Trace (traceShowId, traceShowM)

type Engraver = TryRenderContext (LayoutTree RenderObject)

type HeightMapContextT a = StateT HeightMap a
type HeightMap = IntervalMap (Interval Double) Double

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

    engravedVoices <-
        for (zip3 elementsByLine spanGroups boxesByLine) $ \(elements, spans, boxes) -> do
            engravedBaseNotes <- engraveBaseNotes slicesOffsetX elements
            engravedBeams <- engraveBeams slicesOffsetX spans elements
            engravedSpans <- engraveSpans slicesOffsetX spans boxes
            pure [engravedSpans, engravedBaseNotes, engravedBeams]

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

engraveBeams :: [Double] -> Spans -> [SliceElement] -> Engraver
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
        map (\case I (a, b) -> (a, b)) $
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

engraveSpans ::
    [Double] ->
    IntervalMap (Interval Int) Span ->
    [BoundingBox] ->
    TryRenderContext (LayoutTree RenderObject)
engraveSpans slicesOffsetX spans boxes =
    LTNode mempty <$> engravedSpansTrees
  where
    engravedSpansTrees = evalStateT engravedSpansStateful initialHeightMap

    engravedSpansStateful =
        sequence
            [ engraveSpan absoluteInterval s
            | (I (a, b), s) <- spansInRenderOrder
            , let absoluteInterval =
                    I (slicesOffsetXArray ! a, slicesOffsetXArray ! b)
            ]

    spansInRenderOrder = sortBy (compare `on` SRO) (IM.toList spans)

    slicesOffsetXArray = listArray (0, length slicesOffsetX - 1) slicesOffsetX

    initialHeightMap =
        IM.fromList $
            mapMaybe
                ( \case
                    (i, NoBox) -> Nothing
                    (i, BBox ((x1, y1), (x2, _))) ->
                        Just
                            ( I
                                ( (slicesOffsetXArray ! i) + x1
                                , (slicesOffsetXArray ! i) + x2
                                )
                            , y1
                            )
                )
                (zip [0 ..] boxes)

engraveSpan ::
    Interval Double ->
    Span ->
    HeightMapContextT TryRenderContext (LayoutTree RenderObject)
engraveSpan interval sp = do
    heightMap <- get
    let freeHeight = minimum (0 : IM.elems (heightMap `IM.intersecting` interval))
    engravedSpan <-
        case sp of
            Slur -> engraveSlur interval freeHeight
            Beam -> pure $ LTNode mempty []
    ((x1, y1), (x2, _)) <- boxBound <$> lift (fill (getBox engravedSpan))
    put (IM.insert (I (x1, x2)) y1 heightMap)

    pure engravedSpan

engraveSlur ::
    (MonadReader RenderConfig m) =>
    Interval Double ->
    Double ->
    m (LayoutTree RenderObject)
engraveSlur (I (a, b)) height = do
    slurHeight <- asks getSlurHeight
    slurPaddingX <- asks getSlurPaddingX
    slurPaddingBottom <- asks getSlurPaddingBottom
    pure $
        LTNode
            (move (a + slurPaddingX) (height - slurPaddingBottom))
            [LTLeaf APBottomLeft (Curve (b - a - 2 * slurPaddingX) slurHeight)]
