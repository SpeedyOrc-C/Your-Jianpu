module Data.Jianpu.Abstract.ExtractTagSpans where

import Control.Monad.State
import Data.IntervalMap (Interval (..), IntervalMap)
import Data.IntervalMap qualified as IM
import Data.Jianpu.Abstract.Types
import Data.Jianpu.Syntax.Types qualified as Syntax
import Data.Maybe
import Data.List.Utils

data TagError
    = ErrorArgsTimeSignature
    | ErrorArgsFermata
    | ErrorArgsBeam
    | ErrorArgsTie
    | ErrorArgsSlur
    | ErrorArgsTuplet
    | ErrorArgsLyrics
    | UnknownTag String
    | TagMismatch
    deriving (Show)

extractArg0 :: TagSpan -> TagError -> [Syntax.Argument] -> Either TagError TagSpan
extractArg0 constructor error' = \case
    [] -> Right constructor
    _ -> Left error'

extractTagSpan :: String -> [Syntax.Argument] -> Either TagError TagSpan
extractTagSpan "fermata" = extractArg0 Fermata ErrorArgsFermata
extractTagSpan "beam" = extractArg0 Beam ErrorArgsBeam
extractTagSpan "tie" = extractArg0 Tie ErrorArgsTie
extractTagSpan "slur" = extractArg0 Slur ErrorArgsSlur
extractTagSpan "tuplet" = \case
    [Syntax.Int count] -> Right $ Tuplet count
    _ -> Left ErrorArgsTuplet
extractTagSpan "lyrics" =
    Right . Lyrics . mapMaybe (\case Syntax.String s -> Just s; _ -> Nothing)
extractTagSpan tag = const . Left $ UnknownTag tag

type TagStartEntry =
    ( Int -- position
    , String -- name
    , Maybe Int -- ID
    , [Syntax.Argument]
    )

data ExtractorState = ES
    { currentIndex :: Int
    , spans :: IntervalMap Int TagSpan
    , tagStartStack :: [TagStartEntry]
    }
    deriving (Show)

oneStepForward :: State ExtractorState ()
oneStepForward = modify $ \es@ES{currentIndex} -> es{currentIndex = currentIndex + 1}

tagMatched :: (String, Maybe Int) -> (Maybe String, Maybe Int) -> Bool
tagMatched _ (Nothing, Nothing) = True
tagMatched (_, Just idx) (Nothing, Just idx') = idx == idx'
tagMatched (name, _) (Just name', Nothing) = name == name'
tagMatched (name, Just idx) (Just name', Just idx') = name == name' && idx == idx'
tagMatched _ _ = False

extractOne :: Syntax.Entity -> State ExtractorState (Either TagError (Maybe Entity))
extractOne Syntax.BeginEndRepeat =
    oneStepForward >> pure (Right . Just $ TagSingleton BeginEndRepeat)
extractOne Syntax.BeginRepeat =
    oneStepForward >> pure (Right . Just $ TagSingleton BeginRepeat)
extractOne Syntax.EndRepeat =
    oneStepForward >> pure (Right . Just $ TagSingleton EndRepeat)
extractOne Syntax.DoubleBarLine =
    oneStepForward >> pure (Right . Just $ TagSingleton DoubleBarLine)
extractOne Syntax.EndSign =
    oneStepForward >> pure (Right . Just $ TagSingleton EndSign)
extractOne Syntax.BarLine =
    oneStepForward >> pure (Right . Just $ TagSingleton BarLine)
extractOne (Syntax.Event event) =
    oneStepForward >> pure (Right . Just $ Event event)
extractOne (Syntax.Tag0 "signature" args) =
    case args of
        [Syntax.Int a, Syntax.Int b] ->
            oneStepForward >> pure (Right . Just $ TagSingleton $ TimeSignature a b)
        _ -> return $ Left ErrorArgsTimeSignature
extractOne (Syntax.Tag0 name _) = pure . Left $ UnknownTag name
extractOne (Syntax.Tag1 name args) =
    case extractTagSpan name args of
        Left error' -> return $ Left error'
        Right tagSpan ->
            state $ \es@ES{currentIndex, spans} ->
                ( return Nothing
                , es
                    { currentIndex
                    , spans =
                        IM.insert
                            (ClosedInterval currentIndex currentIndex)
                            tagSpan
                            spans
                    }
                )
extractOne (Syntax.TagStart name idx args) =
    state $ \es@ES{currentIndex, tagStartStack} ->
        ( Right Nothing
        , es{tagStartStack = (currentIndex, name, idx, args) : tagStartStack}
        )
extractOne (Syntax.TagEnd name idx) = state $ \es@ES{currentIndex, spans, tagStartStack} -> do
    let thatMatchesThis :: TagStartEntry -> Bool
        thatMatchesThis (_, name', idx', _) = tagMatched (name', idx') (name, idx)

    case takeFirst thatMatchesThis tagStartStack of
        Nothing -> (Left TagMismatch, es)
        Just ((startIndex, tagName, _, args), tagStartStack') ->
            case extractTagSpan tagName args of
                Left error' ->
                    ( Left error'
                    , es{tagStartStack = tagStartStack'}
                    )
                Right tagSpan ->
                    ( Right Nothing
                    , es
                        { spans =
                            IM.insert
                                (ClosedInterval startIndex (currentIndex - 1))
                                tagSpan spans
                        , tagStartStack = tagStartStack'
                        }
                    )

extractTagSpans :: [Syntax.Entity] -> Either [TagError] (IntervalMap Int TagSpan, [Entity])
extractTagSpans entities' =
    case errors of
        [] -> Right (tagSpans, entities)
        _ -> Left errors
  where
    initialState = ES{currentIndex = 0, tagStartStack = [], spans = IM.empty}

    (xs, s) = runState (traverse extractOne entities') initialState

    entities = flip mapMaybe xs $ \case
        Right (Just entity) -> Just entity
        _ -> Nothing

    ES{spans = tagSpans} = s

    errors = flip mapMaybe xs $ \case
        Left error' -> Just error'
        _ -> Nothing
