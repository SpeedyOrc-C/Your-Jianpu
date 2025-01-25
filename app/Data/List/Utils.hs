module Data.List.Utils where

import Control.Arrow

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
