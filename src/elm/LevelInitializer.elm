module LevelInitializer exposing (initLevel)

import Actor.Actor as Actor
import Actor.Common as Common
import Actor.Decoder
import Data.Config exposing (Config)
import Data.Position exposing (Position)
import Dict


initLevel : Config -> Actor.LevelConfig -> Actor.Level
initLevel config levelConfig =
    emptyLevel config.width config.height levelConfig.viewPosition
        |> setBackground levelConfig.background
        |> setActors levelConfig


emptyLevel : Int -> Int -> Position -> Actor.Level
emptyLevel width height position =
    { nextActorId = 1
    , actors = Dict.fromList []
    , positionIndex = Dict.fromList []
    , view =
        { position = position
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
            (\( y, line ) accLevel ->
                List.indexedMap
                    (\a b -> ( a, b ))
                    (String.toList line)
                    |> List.foldr
                        (\( x, char ) innerAccLevel ->
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
                                            innerAccLevel
                                            |> Just
                                    )
                                |> Maybe.withDefault innerAccLevel
                        )
                        accLevel
            )
            level
