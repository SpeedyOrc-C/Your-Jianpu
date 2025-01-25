module Data.Jianpu.Graphics.Types where

import Control.Monad.State
import Control.Monad.Writer
import Data.Sequence

data XY = XY Double Double deriving (Show, Eq, Ord)

newtype Box = Box (XY, XY) deriving (Show, Eq)

data Object
    = Node
        { position :: XY
        , children :: [Object]
        }
    | Leaf
        { position :: XY
        , scale :: Double
        , sprite :: Sprite
        }
    deriving (Show, Eq)

data Sprite
    = Glyph0
    | Glyph1
    | Glyph2
    | Glyph3
    | Glyph4
    | Glyph5
    | Glyph6
    | Glyph7
    | GlyphX
    | SmallDot
    | Underline
    | SignNatural
    | SignSharp
    | SignFlat
    | SignDoubleSharp
    | SignDoubleFlat
    deriving (Show, Eq, Ord)

data WieldStyle
    = LowerRightMoveRight
    | RightMoveRight
    | UpperRightMoveRight
    | LowerLeftMoveLeft
    | LeftMoveLeft
    | UpperLeftMoveLeft
    | UpperLeftMoveUp
    | UpMoveUp
    | UpperRightMoveUp
    | LowerLeftMoveDown
    | DownMoveDown
    | LowerRightMoveDown
    deriving (Show, Eq)

type PenWielding a =
    StateT
        XY -- The state is pen's current position.
        (Writer (Seq Object)) -- It draws a sequence of images.
        a

instance Num XY where
    (+) :: XY -> XY -> XY
    XY x1 y1 + XY x2 y2 = XY (x1 + x2) (y1 + y2)

    (-) :: XY -> XY -> XY
    XY x1 y1 - XY x2 y2 = XY (x1 - x2) (y1 - y2)

    (*) :: XY -> XY -> XY
    XY x1 y1 * XY x2 y2 = XY (x1 * x2) (y1 * y2)

    abs :: XY -> XY
    abs (XY x y) = XY (abs x) (abs y)

    signum :: XY -> XY
    signum (XY x y) = XY (signum x) (signum y)

    fromInteger :: Integer -> XY
    fromInteger n = XY (fromInteger n) 0
