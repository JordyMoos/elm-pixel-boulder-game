module GameState.PlayingLevel.Animation.PseudoRandomTraversalAnimation exposing (init)

import Data.Config as Config exposing (Config)
import Data.Position as Position exposing (Position)
import Util.PrimeSearch as PrimeSearch


init : Config -> PrimeSearch.Coefficients -> List Position
init config coefficients =
    PrimeSearch.primeSearch
        coefficients
        (config.width * config.height)
        |> List.map
            (\number ->
                { x = (number - 1) % config.width
                , y = (number - 1) // config.height
                }
            )
