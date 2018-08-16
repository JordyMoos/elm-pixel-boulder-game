module GameState.PlayingLevel.Animation.CurrentTick exposing (init)

import Data.Config as Config exposing (Config)
import Data.Position as Position exposing (Position)
import GameState.PlayingLevel.Animation.PseudoRandomTraversal as PseudoRandomTraversal


init : Config -> Int -> List Position
init config currentTick =
    PseudoRandomTraversal.init
        { a = currentTick
        , b = (currentTick % 9999) + 1
        , c = (currentTick % 4242) + 1
        }
        config
        currentTick
