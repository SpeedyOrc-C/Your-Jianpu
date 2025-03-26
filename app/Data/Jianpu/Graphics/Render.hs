{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Data.Jianpu.Graphics.Render where

import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable
import Data.Jianpu.Graphics.Types
import Data.Jianpu.Graphics.XML
import Data.List.NonEmpty (NonEmpty (..))
import Data.Sequence (Seq)
import Data.Sequence qualified as Sq
import Data.Set qualified as S
import Text.XML.HaXml qualified as X
import Data.Jianpu.Types

boundingBox :: Object -> Box
boundingBox Leaf{sprite} = Box (XY 0 0, spriteSize sprite)
boundingBox Node{children} = foldl1 boundingBoxUnion boundingBoxes
  where
    boundingBoxes :: [Box]
    boundingBoxes =
        zipWith
            (\(Box (box1, box2)) pos -> Box (pos + box1, pos + box2))
            (map boundingBox children)
            (map position children)

    boundingBoxUnion :: Box -> Box -> Box
    boundingBoxUnion
        (Box (XY ax1 ay1, XY ax2 ay2))
        (Box (XY bx1 by1, XY bx2 by2)) =
            Box (XY (min ax1 bx1) (min ay1 by1), XY (max ax2 bx2) (max ay2 by2))

spriteSize :: Sprite -> XY
spriteSize = \case
    Glyph0 -> XY 75 100
    Glyph1 -> XY 75 100
    Glyph2 -> XY 75 100
    Glyph3 -> XY 75 100
    Glyph4 -> XY 75 100
    Glyph5 -> XY 75 100
    Glyph6 -> XY 75 100
    Glyph7 -> XY 75 100
    GlyphX -> XY 75 100
    SmallDot -> XY 20 20
    Underline -> XY 75 10
    SignNatural -> XY 35 70
    SignSharp -> XY 35 70
    SignFlat -> XY 35 70
    SignDoubleSharp -> XY 35 70
    SignDoubleFlat -> XY 35 70

draw :: Sprite -> WieldStyle -> PenWielding ()
draw sprite style = do
    let XY width height = spriteSize sprite

    position <- get

    let positionOffset = case style of
            LowerRightMoveRight -> XY 0 0
            RightMoveRight -> XY 0 (-(height / 2))
            UpperRightMoveRight -> XY 0 (-height)
            LowerLeftMoveLeft -> XY (-width) 0
            LeftMoveLeft -> XY (-width) (-(height / 2))
            UpperLeftMoveLeft -> XY (-width) (-height)
            UpperLeftMoveUp -> XY (-width) (-height)
            UpMoveUp -> XY (-(width / 2)) (-height)
            UpperRightMoveUp -> XY 0 (-height)
            LowerLeftMoveDown -> XY (-width) 0
            DownMoveDown -> XY (-(width / 2)) 0
            LowerRightMoveDown -> XY 0 0

    lift . tell . return $
        Leaf{position = position + positionOffset, sprite, scale = 1}

    let penOffset = case style of
            LowerRightMoveRight -> XY width 0
            RightMoveRight -> XY width 0
            UpperRightMoveRight -> XY width 0
            LowerLeftMoveLeft -> XY (-width) 0
            LeftMoveLeft -> XY (-width) 0
            UpperLeftMoveLeft -> XY (-width) 0
            UpperLeftMoveUp -> XY 0 (-height)
            UpMoveUp -> XY 0 (-height)
            UpperRightMoveUp -> XY 0 (-height)
            LowerLeftMoveDown -> XY 0 height
            DownMoveDown -> XY 0 height
            LowerRightMoveDown -> XY 0 height

    modify (+ penOffset)

runPenWielding :: PenWielding a -> Seq Object
runPenWielding penWielding = execWriter $ execStateT penWielding (XY 0 0)

propagateGlobalCoordinate :: XY -> Object -> Object
propagateGlobalCoordinate offset Leaf{position, scale, sprite} =
    Leaf
        { position = position + offset
        , scale
        , sprite
        }
propagateGlobalCoordinate offset Node{position, children} =
    Node
        { position = position'
        , children = map (propagateGlobalCoordinate position') children
        }
  where
    position' = position + offset

leavesOf :: Object -> Seq (XY, Double, Sprite)
leavesOf Leaf{position, scale, sprite} = Sq.singleton (position, scale, sprite)
leavesOf Node{children} = msum (leavesOf <$> children)

symbolOf :: Sprite -> X.Element ()
symbolOf = \case
    Glyph0 -> glyph0
    Glyph1 -> glyph1
    Glyph2 -> glyph2
    Glyph3 -> glyph3
    Glyph4 -> glyph4
    Glyph5 -> glyph5
    Glyph6 -> glyph6
    Glyph7 -> glyph7
    GlyphX -> glyphX
    SmallDot -> smallDot
    Underline -> underline
    SignNatural -> accidentalNatural
    SignSharp -> accidentalSharp
    SignFlat -> accidentalFlat

useOf :: (XY, Double, Sprite) -> X.Content ()
useOf (XY (show -> x) (show -> y), show -> scale, show -> sprite) =
    X.CElem
        ( X.Elem
            (X.N "use")
            [ (X.N "href", X.AttValue [Left $ '#' : sprite])
            , (X.N "transform", X.AttValue [Left matrix])
            ]
            []
        )
        ()
  where
    matrix = "matrix(" ++ scale ++ ",0,0," ++ scale ++ "," ++ x ++ "," ++ y ++ ")"

render :: Object -> X.Document ()
render node =
    X.Document
        (X.Prolog (Just (X.XMLDecl "1.0" (Just (X.EncodingDecl "UTF-8")) Nothing)) [] Nothing [])
        []
        ( X.Elem
            (X.N "svg")
            [ (X.N "viewBox", X.AttValue [Left . unwords . map show $ [x1, y1, x2 - x1, y2 - y1]])
            , (X.N "xmlns", X.AttValue [Left "http://www.w3.org/2000/svg"])
            ]
            ( X.CElem (X.Elem (X.N "defs") [] defs) ()
                : uses
            )
        )
        []
  where
    leaves = toList . leavesOf $ propagateGlobalCoordinate (XY 0 0) node
    usedSprites = toList . S.fromList $ (\(_, _, sprite) -> sprite) <$> leaves
    defs = (`X.CElem` ()) . symbolOf <$> usedSprites
    uses = useOf <$> leaves
    Box (XY x1 y1, XY x2 y2) = boundingBox node

resetPen :: PenWielding ()
resetPen = modify (const (XY 0 0))

moveRight :: (MonadState XY m) => Double -> m ()
moveRight d = modify (+ XY d 0)
moveLeft :: (MonadState XY m) => Double -> m ()
moveLeft d = modify (+ XY (-d) 0)
moveUp :: (MonadState XY m) => Double -> m ()
moveUp d = modify (+ XY 0 (-d))
moveDown :: (MonadState XY m) => Double -> m ()
moveDown d = modify (+ XY 0 d)

scopedState :: (MonadState s m) => m a -> m ()
scopedState inner = do
    oldState <- get
    void inner
    put oldState

drawMusicalEvent :: Event -> PenWielding ()
drawMusicalEvent (Action multiplier rightDotsCount sound) = do
    put $ XY 57.5 (-50)
    replicateM_ rightDotsCount $ do
        draw SmallDot RightMoveRight
        moveRight 20

    let underlinesCount = case multiplier of
            Whole -> 0
            Minim -> 1
            Crotchet -> 2
            Quaver -> 3
            Semiquaver -> 4

    resetPen
    replicateM_ underlinesCount $ do
        moveDown 10
        draw Underline DownMoveDown

    case sound of
        Note (Pitch _ t _ :| _) _ | t < 0 -> do
            replicateM_ (-t) $ do
                moveDown 10
                draw SmallDot DownMoveDown
        _ -> return ()

    resetPen
    case sound of
        Clap -> draw GlyphX UpMoveUp
        Rest -> draw Glyph0 UpMoveUp
        Note (Pitch firstKey firstTranspose firstAccidental :| pitches) _ -> do
            case firstKey of
                K1 -> draw Glyph1 UpMoveUp
                K2 -> draw Glyph2 UpMoveUp
                K3 -> draw Glyph3 UpMoveUp
                K4 -> draw Glyph4 UpMoveUp
                K5 -> draw Glyph5 UpMoveUp
                K6 -> draw Glyph6 UpMoveUp
                K7 -> draw Glyph7 UpMoveUp

            scopedState $ do
                put $ XY (-37.5) (-100)
                case firstAccidental of
                    Just Natural -> draw SignNatural LowerLeftMoveLeft
                    Just Sharp -> draw SignSharp LowerLeftMoveLeft
                    Just Flat -> draw SignFlat LowerLeftMoveLeft
                    Nothing -> return ()

            when (firstTranspose > 0) $ do
                replicateM_ firstTranspose $ do
                    moveUp 10
                    draw SmallDot UpMoveUp

            moveUp 20

            for_ pitches $ \(Pitch key t accidental) -> do
                when (t < 0) $ do
                    replicateM_ (-t) $ do
                        moveUp 10
                        draw SmallDot UpMoveUp
                moveUp 10

                case key of
                    K1 -> draw Glyph1 UpMoveUp
                    K2 -> draw Glyph2 UpMoveUp
                    K3 -> draw Glyph3 UpMoveUp
                    K4 -> draw Glyph4 UpMoveUp
                    K5 -> draw Glyph5 UpMoveUp
                    K6 -> draw Glyph6 UpMoveUp
                    K7 -> draw Glyph7 UpMoveUp

                scopedState $ do
                    modify $ \(XY _ y) -> XY (-37.5) y
                    case accidental of
                        Just Natural -> draw SignNatural LowerLeftMoveLeft
                        Just Sharp -> draw SignSharp LowerLeftMoveLeft
                        Just Flat -> draw SignFlat LowerLeftMoveLeft
                        Nothing -> return ()

                when (t > 0) $ do
                    replicateM_ t $ do
                        moveUp 10
                        draw SmallDot UpMoveUp

                moveUp 20
