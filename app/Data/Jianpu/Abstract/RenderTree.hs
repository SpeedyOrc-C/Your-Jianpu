module Data.Jianpu.Abstract.RenderTree (voicesTreeFromMusic) where

import Control.Monad.Reader (asks)
import Data.Function (on)
import Data.IntervalMap (Interval (ClosedInterval))
import Data.IntervalMap qualified as IM
import Data.Jianpu.Abstract (Entity (Event, event), Music, Span (Beam))
import Data.Jianpu.Abstract.Error (HasError)
import Data.Jianpu.Graphics (Slice (elements), SliceElement, Slices (..))
import Data.Jianpu.Graphics.Config (RenderConfig (lineGap, lineWidth), RenderContextT, fill, getBeamGap, getBeamHeight, getGlyphWidth)
import Data.Jianpu.Graphics.Render (RenderObject (Rectangle), drawSliceElement)
import Data.Jianpu.Graphics.Slice (sliceMusic)
import Data.Jianpu.Graphics.Spacing (SpringWithRod (..), computeSpringConstants, sff)
import Data.Jianpu.Types (Event (Action, timeMultiplier))
import Data.Layout (AnchorPosition (APTopLeft), HasBox (getBox), LayoutTree (..), boxBoundX, boxBoundY, move, moveDown, moveRight)
import Data.List (groupBy, transpose, zip4)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)

voicesTreeFromMusic :: Music -> RenderContextT HasError (LayoutTree RenderObject)
voicesTreeFromMusic music = do
    let Slices slices spanGroups = sliceMusic music

    slicesTrees <- mapM (mapM drawSliceElement . elements) slices
    slicesBoxes <- mapM (mapM (fill . getBox)) slicesTrees

    let slicesElementsBoundsY = map (map boxBoundX) slicesBoxes

    let springMinLengths =
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

    let springConstants = (1 / 0) : computeSpringConstants slices

    let springs = zipWith SWR springMinLengths springConstants

    lineWidth <- asks lineWidth

    let force = sff (NE.sort (NE.fromList springs)) lineWidth

    let springExtensions =
            [max rodLength (force / springConst) | SWR{..} <- springs]

    let voicesBoxes = transpose slicesBoxes

    let voicesBoundsY = boxBoundY . mconcat <$> voicesBoxes

    let voicesTrees = transpose slicesTrees

    let slicesOffsetX = init $ scanl1 (+) springExtensions

    lineGap <- asks lineGap

    let voicesOffsetsY =
            scanl1 (+) $
                negativeSize (head voicesBoundsY)
                    : zipWith
                        ( \upperLineBottom lowerLineTop ->
                            upperLineBottom + lineGap + lowerLineTop
                        )
                        (positiveSize <$> voicesBoundsY)
                        (negativeSize <$> tail voicesBoundsY)

    let voicesElements = transpose $ elements <$> slices

    glyphWidth <- asks getGlyphWidth
    beamHeight <- asks getBeamHeight
    beamGap <- asks getBeamGap

    pure $ LTNode mempty $ do
        (voiceTrees, voiceOffsetY, spans, voiceElements) <-
            zip4 voicesTrees voicesOffsetsY spanGroups voicesElements

        let maxBeamCount = maximum $ beamCount <$> voiceElements

        let beamMasksByEntity = do
                element <- voiceElements
                let count = beamCount element
                [replicate count True ++ replicate (maxBeamCount - count) False]

        let beamMasksByLevel = transpose beamMasksByEntity

        let beamSpans =
                mapMaybe (\case ClosedInterval a b -> Just (a, b); _ -> Nothing) $
                    IM.keys $
                        IM.mapMaybe (\case Beam -> Just (); _ -> Nothing) spans

        let inTheSameBeam a b =
                any (\(spanA, spanB) -> spanA <= a && a <= b && b <= spanB) beamSpans

        let beamsTree = LTNode mempty $ do
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

        pure $
            LTNode (moveDown voiceOffsetY) $
                beamsTree : do
                    (voiceElement, sliceOffsetX) <- zip voiceTrees slicesOffsetX
                    pure $ LTNode (moveRight sliceOffsetX) [voiceElement]

beamCount :: SliceElement -> Int
beamCount (Just (Right (Event{event = Action{..}}))) = fromEnum timeMultiplier
beamCount _ = 0

negativeSize :: (Double, Double) -> Double
negativeSize (x, _) = max 0 (-x)

positiveSize :: (Double, Double) -> Double
positiveSize (_, x) = max 0 x