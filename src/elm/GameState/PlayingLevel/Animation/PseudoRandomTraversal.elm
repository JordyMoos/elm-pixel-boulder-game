module GameState.PlayingLevel.Animation.PseudoRandomTraversal exposing (init)

import Data.Config as Config exposing (Config)
import Data.Position as Position exposing (Position)
import Util.PrimeSearch as PrimeSearch


init : PrimeSearch.Coefficients -> Config -> Int -> List Position
init coefficients config currentTick =
    PrimeSearch.primeSearch
        coefficients
        (config.width * config.height)
        |> List.map
            (\number ->
                { x = (number - 1) % config.width
                , y = (number - 1) // config.height
                }
            )
