module GameState.PlayingLevel exposing (..)

import Data.Config exposing (Config)
import Actor
import Dict


type alias Model =
    { config : Config
    , levelConfig : Actor.LevelConfig
    , images : Actor.CanvasImages
    , level : Actor.Level
    }


init : Config -> Actor.LevelConfig -> Actor.CanvasImages -> Model
init config levelConfig images =
    { config = config
    , levelConfig = levelConfig
    , images = images
    , level = createLevel config levelConfig
    }


createLevel : Config -> Actor.LevelConfig -> Actor.Level
createLevel config levelConfig =
    emptyLevel config.width config.height
        |> setBackground config.background
        |> setActors config.scene


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
    , background = Actor.defaultBackground
    }


setBackground : Actor.RenderComponentData -> Actor.Level -> Actor.Level
setBackground background level =
    { level | background = background }


setActors : Actor.Scene -> Actor.Level -> Actor.Level
setActors scene level =
    List.indexedMap
        (,)
        scene
        |> List.foldr
            (\( y, line ) level ->
                List.indexedMap
                    (,)
                    (String.toList line)
                    |> List.foldr
                        (\( x, char ) level ->
                            Dict.get
                                (String.fromChar char)
                                level.signs
                                |> Maybe.andThen
                                    (\entityName ->
                                        Dict.get entityName level.entities
                                    )
                                |> Maybe.andThen
                                    (\entity ->
                                        addActor
                                            (Dict.insert
                                                "transform"
                                                (TransformComponent { position = { x = x, y = y }, movingState = NotMoving })
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



{-
