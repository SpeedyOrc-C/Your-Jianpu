module Data.Layout where

import Control.Monad.Reader
import Data.Monoid

type XY = (Double, Double)
type Size = (Double, Double)

data Transform = Transform
    { localPosition :: XY
    , localScale :: XY
    }
    deriving (Show)

data BoundingBox = BBox (XY, XY) | NoBox
    deriving (Show, Eq)

data AnchorPosition
    = APCentre
    | APBottom
    | APLeft
    | APRight
    | APTop
    | APTopRight
    | AP XY
    deriving (Show)

data LayoutTree a
    = LTLeaf AnchorPosition a
    | LTNode Transform [LayoutTree a]
    deriving (Show)

newtype LayoutFlat a = LayoutFlat [(Transform, AnchorPosition, a)]
    deriving (Show)

class HasSize config a where
    getSize :: a -> Reader config BoundingBox

flattenLayoutTree :: LayoutTree a -> [(Transform, AnchorPosition, a)]
flattenLayoutTree tree =
    appEndo (flattenLayoutTree' (Transform (0, 0) (1, 1)) tree) []

flattenLayoutTree' ::
    Transform -> LayoutTree a -> Endo [(Transform, AnchorPosition, a)]
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

instance Semigroup BoundingBox where
    (<>) :: BoundingBox -> BoundingBox -> BoundingBox
    NoBox <> box = box
    box <> NoBox = box
    (BBox ((ax1, ay1), (ax2, ay2))) <> (BBox ((bx1, by1), (bx2, by2))) =
        BBox ((min ax1 bx1, min ay1 by1), (min ax2 bx2, min ay2 by2))

instance Monoid BoundingBox where
    mempty :: BoundingBox
    mempty = NoBox

instance Semigroup Transform where
    (<>) :: Transform -> Transform -> Transform
    Transform (x1, y1) (sx1, sy1) <> Transform (x2, y2) (sx2, sy2) =
        Transform (x1 + x2 * sx1, y1 + y2 * sy1) (sx1 * sx2, sy1 * sy2)

instance Monoid Transform where
    mempty :: Transform
    mempty = Transform (0, 0) (1, 1)

instance (HasSize config a) => HasSize config (LayoutTree a) where
    getSize :: (HasSize config a) => LayoutTree a -> Reader config BoundingBox
    getSize (LTLeaf anchorAlignment a) = do
        box <- getSize a
        pure $ case box of
            NoBox -> NoBox
            BBox ((bx1, by1), (bx2, by2)) ->
                BBox
                    ( (-(ax * width), -(ay * height))
                    , ((1 - ax) * width, (1 - ay) * height)
                    )
              where
                width = bx2 - bx1
                height = by2 - by1
                (ax, ay) = case anchorAlignment of
                    AP anchor -> anchor
                    APCentre -> (0.5, 0.5)
    getSize (LTNode (Transform (dx, dy) (sx, sy)) ts) = do
        boxes <- traverse getSize ts
        pure $ case mconcat boxes of
            NoBox -> NoBox
            BBox ((x1, y1), (x2, y2)) ->
                BBox
                    ( (x1 * sx + dx, y1 * sy + dy)
                    , (x2 * sx + dx, y2 * sy + dy)
                    )
