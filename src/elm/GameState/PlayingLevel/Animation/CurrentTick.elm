module GameState.PlayingLevel.Animation.CurrentTick exposing (init)

import Data.Config as Config exposing (Config)
import Data.Position as Position exposing (Position)
import GameState.PlayingLevel.Animation.PseudoRandomTraversal as PseudoRandomTraversal


init : Config -> Int -> List Position
init config currentTick =
    PseudoRandomTraversal.init
        { a = currentTick
        , b = (modBy 9999 currentTick) + 1
        , c = (modBy 4242 currentTick) + 1
        }
        config
        currentTick
