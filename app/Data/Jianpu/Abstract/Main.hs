module Data.Jianpu.Abstract.Main where

import Data.Jianpu.Abstract.Error
import Data.Jianpu.Abstract.ExtractTagSpans
import Data.Jianpu.Abstract.GenerateLyricsVoices
import Data.Jianpu.Abstract.Types
import Data.Jianpu.Syntax.Types qualified as Syntax
import Data.Jianpu.Abstract.CalculateTiming

abstractMain :: Syntax.Music -> Either [AbstractError] Music
abstractMain (Syntax.Music syntaxVoices) = do
    spansAndEvents <- traverse (extractTagSpans . Syntax.entities) syntaxVoices

    let entitiesGroups = calculateDurations <$> spansAndEvents

    lyricsVoiceGroups <- sequence $ generateLyricsVoices syntaxVoices entitiesGroups

    let spanGroups = fst <$> spansAndEvents

    let normalVoices = zipWith Voice entitiesGroups spanGroups

    pure $ Music $ concat (zipWith (:) normalVoices lyricsVoiceGroups)
