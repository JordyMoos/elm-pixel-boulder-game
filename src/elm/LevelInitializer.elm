module LevelInitializer exposing (initLevel)

import Actor.Actor as Actor
import Actor.Common as Common
import Actor.Decoder
import Data.Config exposing (Config)
import Dict


initLevel : Config -> Actor.LevelConfig -> Actor.Level
initLevel config levelConfig =
    emptyLevel config.width config.height
        |> setBackground levelConfig.background
        |> setActors levelConfig


emptyLevel : Int -> Int -> Actor.Level
emptyLevel width height =
    { nextActorId = 1
    , actors = Dict.fromList []
    , positionIndex = Dict.fromList []
    , view =
        { position = { x = 0, y = 0 }
        , width = width
        , height = height
        }
    , background = Actor.Decoder.defaultBackground
    , events = []
    }


setBackground : Actor.RenderComponentData -> Actor.Level -> Actor.Level
setBackground background level =
    { level | background = background }


setActors : Actor.LevelConfig -> Actor.Level -> Actor.Level
setActors levelConfig level =
    List.indexedMap
        (\a b -> ( a, b ))
        levelConfig.scene
        |> List.foldr
            (\( y, line ) level ->
                List.indexedMap
                    (\a b -> ( a, b ))
                    (String.toList line)
                    |> List.foldr
                        (\( x, char ) level ->
                            Dict.get
                                (String.fromChar char)
                                levelConfig.signs
                                |> Maybe.andThen
                                    (\entityName ->
                                        Dict.get entityName levelConfig.entities
                                    )
                                |> Maybe.andThen
                                    (\entity ->
                                        Common.addActor
                                            (Dict.insert
                                                "transform"
                                                (Actor.TransformComponent { position = { x = x, y = y }, movingState = Actor.NotMoving })
                                                entity
                                            )
                                            level
                                            |> Just
                                    )
                                |> Maybe.withDefault level
                        )
                        level
            )
            level
