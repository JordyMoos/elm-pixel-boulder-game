module GameState.PlayingLevel.Animation.ReadingDirectionAnimation exposing (init)

import Data.Config as Config exposing (Config)
import Data.Position as Position exposing (Position)


init : Config -> List Position
init config =
    List.concatMap
        (\y ->
            List.map
                (\x ->
                    { x = x, y = y }
                )
                (List.range 0 <| config.width - 1)
        )
        (List.range 0 <| config.height - 1)
