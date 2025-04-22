module Data.Layout where

import Control.Monad.Reader
import Data.Monoid

type XY = (Double, Double)
type Size = (Double, Double)

data Transform = Transform
    { localPosition :: XY
    , localScale :: XY
    }

data BoundingBox = BBox BBox | NoBox
    deriving (Show, Eq)

type BBox = (XY, XY)

data AnchorPosition
    = APCentre
    | APBottom
    | APLeft
    | APRight
    | APTop
    | APTopLeft
    | APTopRight
    | AP XY
    deriving (Show)

data LayoutTree a
    = LTLeaf AnchorPosition a
    | LTNode Transform [LayoutTree a]

newtype LayoutFlat a = LayoutFlat [(Transform, AnchorPosition, a)]
    deriving (Show)

type DrawDirective a = (Transform, AnchorPosition, a)

class HasSize config a where
    getSize :: a -> Reader config Size

class HasBox config a where
    getBox :: a -> Reader config BoundingBox

boxBoundX :: BoundingBox -> (Double, Double)
boxBoundX NoBox = (0, 0)
boxBoundX (BBox ((x1, _), (x2, _))) = (x1, x2)

boxBoundY :: BoundingBox -> (Double, Double)
boxBoundY NoBox = (0, 0)
boxBoundY (BBox ((_, y1), (_, y2))) = (y1, y2)

flattenLayoutTree :: LayoutTree a -> [DrawDirective a]
flattenLayoutTree tree =
    appEndo (flattenLayoutTree' (Transform (0, 0) (1, 1)) tree) []

flattenLayoutTree' ::
    Transform -> LayoutTree a -> Endo [DrawDirective a]
flattenLayoutTree' transform = \case
    LTLeaf anchorAlignment object ->
        Endo ((transform, anchorAlignment, object) :)
    LTNode transform' trees ->
        mconcat $ flattenLayoutTree' (transform <> transform') <$> trees

move :: Double -> Double -> Transform
moveUp :: Double -> Transform
moveDown :: Double -> Transform
moveLeft :: Double -> Transform
moveRight :: Double -> Transform
move dx dy = Transform (dx, dy) (1, 1)
moveUp dy = Transform (0, -dy) (1, 1)
moveDown dy = Transform (0, dy) (1, 1)
moveLeft dx = Transform (-dx, 0) (1, 1)
moveRight dx = Transform (dx, 0) (1, 1)

computeBox :: XY -> Size -> AnchorPosition -> BBox
computeBox (x, y) (sx, sy) anchorPosition =
    ((x + dx, y + dy), (x + sx + dx, y + sy + dy))
  where
    dx = -(px * sx)
    dy = -(py * sy)
    (px, py) = normAnchorPosition anchorPosition

normAnchorPosition :: AnchorPosition -> XY
normAnchorPosition (AP xy) = xy
normAnchorPosition APCentre = (0.5, 0.5)
normAnchorPosition APTop = (0.5, 0)
normAnchorPosition APBottom = (0.5, 1)
normAnchorPosition APLeft = (0, 0.5)
normAnchorPosition APRight = (1, 0.5)
normAnchorPosition APTopRight = (1, 0)
normAnchorPosition APTopLeft = (0, 0)

instance Semigroup BoundingBox where
    (<>) :: BoundingBox -> BoundingBox -> BoundingBox
    NoBox <> box = box
    box <> NoBox = box
    (BBox ((ax1, ay1), (ax2, ay2))) <> (BBox ((bx1, by1), (bx2, by2))) =
        BBox ((min ax1 bx1, min ay1 by1), (max ax2 bx2, max ay2 by2))

instance Monoid BoundingBox where
    mempty :: BoundingBox
    mempty = NoBox

instance Show Transform where
    show :: Transform -> String
    show (Transform (0, 0) (1, 1)) = "I"
    show (Transform (x, y) (sx, sy)) =
        show (x, y) ++ "~" ++ show (sx, sy)

instance Semigroup Transform where
    (<>) :: Transform -> Transform -> Transform
    Transform (x1, y1) (sx1, sy1) <> Transform (x2, y2) (sx2, sy2) =
        Transform (x1 + x2 * sx1, y1 + y2 * sy1) (sx1 * sx2, sy1 * sy2)

instance Monoid Transform where
    mempty :: Transform
    mempty = Transform (0, 0) (1, 1)

instance (Show a) => Show (LayoutTree a) where
    show :: (Show a) => LayoutTree a -> String
    show (LTNode _ []) = ""
    show (LTNode transform trees) = show transform ++ ":" ++ show trees
    show (LTLeaf anchorAlignment object) =
        show anchorAlignment ++ ":" ++ show object

instance (HasSize config a) => HasBox config (DrawDirective a) where
    getBox :: (HasSize config a) => DrawDirective a -> Reader config BoundingBox
    getBox (transform, anchorAlignment, a) =
        getBox $ LTNode transform [LTLeaf anchorAlignment a]

instance (HasSize config a) => HasBox config (LayoutFlat a) where
    getBox (LayoutFlat ts) = undefined <$> mapM getBox ts

instance (HasSize config a) => HasBox config (LayoutTree a) where
    getBox :: (HasSize config a) => LayoutTree a -> Reader config BoundingBox
    getBox (LTLeaf anchorAlignment a) = do
        let (ax, ay) = normAnchorPosition anchorAlignment
        (width, height) <- getSize a
        pure . BBox $
            ( (-(ax * width), -(ay * height))
            , ((1 - ax) * width, (1 - ay) * height)
            )
    getBox (LTNode (Transform (dx, dy) (sx, sy)) ts) = do
        boxes <- traverse getBox ts
        pure $ case mconcat boxes of
            NoBox -> NoBox
            BBox ((x1, y1), (x2, y2)) ->
                BBox
                    ( (x1 * sx + dx, y1 * sy + dy)
                    , (x2 * sx + dx, y2 * sy + dy)
                    )

instance (HasSize config a) => HasBox config [LayoutTree a] where
    getBox :: (HasSize config a) => [LayoutTree a] -> Reader config BoundingBox
    getBox = fmap mconcat . mapM getBox
