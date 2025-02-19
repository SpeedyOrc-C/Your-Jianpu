module Main where

import Data.Jianpu.Abstract.CalculateTiming
import Data.Jianpu.Abstract.ExtractTagSpans
import Data.Jianpu.Graphics.TextMetric qualified as TM
import Data.Jianpu.Syntax.Parser
import Data.Jianpu.Syntax.Types qualified as S
import Data.Jianpu.Abstract.Types
import Data.Jianpu.Transform.Lyrics

test s =
    expandLyrics voice
  where
    Right (S.Voice syntaticEntities) = run pVoice s
    Right (tagSpans, entities) = extractTagSpans syntaticEntities
    durations = calculateTiming tagSpans entities
    voice = Voice (zipWith VoiceItem entities durations) tagSpans

main :: IO ()
main = do
    result <-
        TM.fetch $
            TM.Request
                { TM.fontSize = 24
                , TM.font = "-apple-system"
                , TM.strings =
                    [ "我"
                    , "我不"
                    , "我不是"
                    , "我不是人"
                    ]
                }

    print result
