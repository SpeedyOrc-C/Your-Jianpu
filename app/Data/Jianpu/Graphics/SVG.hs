module Data.Jianpu.Graphics.SVG where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.List.NonEmpty (NonEmpty (..))
import Data.Jianpu.Graphics.Config
import Data.Jianpu.Graphics.Render
import Data.Jianpu.Types
import Data.Layout

type StringWriter = Writer (Endo String) ()

puts :: String -> StringWriter
puts content = tell $ Endo (content ++)

putImage x y width height href = do
    puts "<image href="
    puts $ show href
    puts " x="
    puts $ show $ show x
    puts " y="
    puts $ show $ show y
    puts " width="
    puts $ show $ show width
    puts " height="
    puts $ show $ show height
    puts " />"

putCircle cx cy r = do
    puts "<circle cx="
    puts $ show $ show cx
    puts " cy="
    puts $ show $ show cy
    puts " r="
    puts $ show $ show r
    puts " />"

putRect x y width height = do
    puts "<rect x="
    puts $ show $ show x
    puts " y="
    puts $ show $ show y
    puts " width="
    puts $ show $ show width
    puts " height="
    puts $ show $ show height
    puts " />"

putGlyph x y width height g = putImage x y width height $ case g of
    G0 -> "lib/Glyph0.svg"
    G1 -> "lib/Glyph1.svg"
    G2 -> "lib/Glyph2.svg"
    G3 -> "lib/Glyph3.svg"
    G4 -> "lib/Glyph4.svg"
    G5 -> "lib/Glyph5.svg"
    G6 -> "lib/Glyph6.svg"
    G7 -> "lib/Glyph7.svg"
    GX -> "lib/GlyphX.svg"

putAccidental x y width height a = putImage x y width height $ case a of
    GSharp -> "lib/AccidentalSharp.svg"
    GFlat -> "lib/AccidentalFlat.svg"
    GNatural -> "lib/AccidentalNatural.svg"
    GDoubleSharp -> "lib/AccidentalDoubleSharp.svg"
    GDoubleFlat -> "lib/AccidentalDoubleFlat.svg"

getSvgString :: (Transform, AnchorPosition, RenderObject) -> Reader RenderConfig StringWriter
getSvgString (Transform position (scaleX, scaleY), anchorPosition, object) = do
    (sizeX, sizeY) <- computeSize object

    let ((x1, y1), (x2, y2)) =
            computeBox position (sizeX * scaleX, sizeY * scaleY) anchorPosition

    let x = x1
    let y = y1
    let width = x2 - x1
    let height = y2 - y1

    pure $ do
        puts "\t"

        case object of
            Circle r -> putCircle ((x1 + x2) / 2) ((y1 + y2) / 2) r
            Rectangle width' height' -> putRect x y width' height'
            Glyph g -> putGlyph x y width height g
            GAccidental a -> putAccidental x y width height a

        puts "\n"

d = writeFile "./svg-output/index.svg" $ (`appEndo` "") . execWriter $ do
    puts "<svg xmlns=\"http://www.w3.org/2000/svg\" viewbox=\"0 0 1000 1000\">\n"

    sequence_ . flip runReader defaultRenderConfig $ do
        tree <- drawEvent (Action Whole 5 (Note (Pitch K6 5 (Just DoubleFlat) :| []) undefined)) -- (Pitch K6 5 (Just DoubleFlat)) 5 Whole
        let flatTree = flattenLayoutTree (LTNode (move 200 200) [tree])
        traverse getSvgString flatTree

    puts "</svg>\n"
