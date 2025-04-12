module Data.Jianpu.Abstract.GenerateLyricsVoices where

import Data.IntervalMap qualified as IM
import Data.Jianpu.Abstract
import Data.Jianpu.Abstract.Error
import Data.Jianpu.Syntax qualified as Syntax
import Data.Jianpu.Types
import Data.Maybe

{-
Generate the boilerplate lyrics voices directly from the melody voice and
their notes' durations.
-}
generateLyricsVoices :: [[[Maybe Syllable]]] -> [[Entity]] -> [Either [AbstractError] [[Entity]]]
generateLyricsVoices lyricsLinesGroups entitiesGroups =
    zipWith generateLyricsVoices' entitiesGroups lyricsLinesGroups

generateLyricsVoices' :: [Entity] -> [[Maybe Syllable]] -> Either [AbstractError] [[Entity]]
generateLyricsVoices' voice lyricsLines =
    if all ((== length durations) . length) lyricsLines
        then Right $ (\line -> zipWith (Event . Pronounce) line durations) <$> lyricsLines
        else Left [InconsistentSyllableCount (length durations)]
  where
    durations = flip mapMaybe voice $ \case
        Event{duration} -> Just duration
        _ -> Nothing
