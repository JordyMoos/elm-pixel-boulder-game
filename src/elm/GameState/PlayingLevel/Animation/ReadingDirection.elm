module GameState.PlayingLevel.Animation.ReadingDirection exposing (init)

import Data.Config as Config exposing (Config)
import Data.Position as Position exposing (Position)


init : Config -> Int -> List Position
init config currentTick =
    List.concatMap
        (\y ->
            List.map
                (\x ->
                    { x = x, y = y }
                )
                (List.range 0 <| config.width - 1)
        )
        (List.range 0 <| config.height - 1)
