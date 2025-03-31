module Data.Jianpu.Abstract.GenerateLyricsVoices where

import Data.Jianpu.Abstract.Error
import Data.Jianpu.Abstract.Types
import Data.Jianpu.Syntax.Types qualified as Syntax
import Data.Jianpu.Types
import Data.IntervalMap qualified as IM
import Data.Maybe

{-
Generate the boilerplate lyrics voices directly from the melody voice and
their notes' durations.
-}
generateLyricsVoices ::
    [Syntax.Voice] ->
    [[Entity]] ->
    [Either [AbstractError] [Voice]]
generateLyricsVoices syntaxVoices entitiesGroups =
    zipWith generateLyricsVoices' entitiesGroups lyricsLineGroups
  where
    lyricsLineGroups = Syntax.lyricsLines <$> syntaxVoices

generateLyricsVoices' ::
    [Entity] ->
    [[Maybe Syllable]] ->
    Either [AbstractError] [Voice]
generateLyricsVoices' voice lyricsLines =
    if all ((== length durations) . length) lyricsLines
        then Right $ (\line -> Voice (zipWith (Event . Pronounce) line durations) IM.empty) <$> lyricsLines
        else Left [InconsistentSyllableCount (length durations)]
  where
    durations = flip mapMaybe voice $ \case
        Event{duration} -> Just duration
        _ -> Nothing
