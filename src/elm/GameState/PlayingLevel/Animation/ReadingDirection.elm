module GameState.PlayingLevel.Animation.ReadingDirection exposing (init)

import Data.Config as Config exposing (Config)
import Data.Position as Position exposing (Position)
import Util.Util as Util


init : Config -> Int -> List Position
init config _ =
    Util.fastConcatMap
        (\y ->
            List.map
                (\x ->
                    { x = x, y = y }
                )
                (List.range 0 <| config.width - 1)
        )
        (List.range 0 <| config.height - 1)
