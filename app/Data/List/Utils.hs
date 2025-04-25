module Data.List.Utils where

import Control.Arrow
import Data.List.NonEmpty (NonEmpty(..))
import Data.Either
import Data.Foldable

takeFirst :: (a -> Bool) -> [a] -> Maybe (a, [a])
takeFirst _ [] = Nothing
takeFirst condition (x : xs) =
    if condition x
        then Just (x, xs)
        else second (x :) <$> takeFirst condition xs

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn sep str@(strHead : strTail)
    | strHead == sep = splitOn sep strTail
    | otherwise =
        let (element, strRest) = span (/= sep) str
         in element : splitOn sep strRest

partitionEithersNE :: NonEmpty (Either a b) -> Either (NonEmpty a) (NonEmpty b)
partitionEithersNE (Left a :| xs) =
    Left $ a :| lefts xs
partitionEithersNE (Right b :| []) =
    Right $ b :| []
partitionEithersNE (Right b :| x:xs) =
    case partitionEithersNE (x :| xs) of
        Right bs -> Right $ b :| toList bs
        as -> as

groupByAdjacent :: (a -> a -> Bool) -> [a] -> [[a]]
groupByAdjacent _ [] = []
groupByAdjacent _ [x] = [[x]]
groupByAdjacent condition (x : xs) =
    let (same, rest) = span (condition x) xs
     in (x : same) : groupByAdjacent condition rest
