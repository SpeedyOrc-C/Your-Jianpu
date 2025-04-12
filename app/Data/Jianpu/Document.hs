module Data.Jianpu.Document where

import Data.IntervalMap (IntervalMap)
import Data.Jianpu.Abstract (Entity, Span)
import Data.Jianpu.Types (Syllable)

newtype DocumentMusic = DMusic [DocumentVoice]

data DocumentVoice
    = DVoice
    { dEntities :: [Entity]
    , dSpans :: IntervalMap Int Span
    , lyrics :: [[Maybe Syllable]]
    }
