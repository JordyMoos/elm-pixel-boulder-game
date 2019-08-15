module LevelInitializer exposing (initLevel)

import Actor.Actor as Actor
import Actor.Common as Common
import Actor.Decoder
import Data.Config exposing (Config)
import Data.Coordinate exposing (Coordinate)
import Dict
import String.Extra


initLevel : Config -> Actor.LevelConfig -> Actor.Level
initLevel config levelConfig =
    emptyLevel (Maybe.withDefault config levelConfig.config) levelConfig.viewCoordinate
        |> setBackgrounds levelConfig.backgrounds
        |> setActors levelConfig
        |> setEventManager levelConfig


emptyLevel : Config -> Coordinate -> Actor.Level
emptyLevel config coordinate =
    { nextActorId = 0
    , actors = Dict.fromList []
    , positionIndices =
        { environment = Dict.fromList []
        , static = Dict.fromList []
        , dynamic = Dict.fromList []
        }
    , view =
        { coordinate = coordinate
        }
    , backgrounds = Actor.Decoder.defaultBackgrounds
    , eventManager = Actor.emptyEventManager
    , events = []
    , config = config
    }


setBackgrounds : List Actor.RenderComponentData -> Actor.Level -> Actor.Level
setBackgrounds backgrounds level =
    { level | backgrounds = backgrounds }


setEventManager : Actor.LevelConfig -> Actor.Level -> Actor.Level
setEventManager levelConfig level =
    { level
        | eventManager =
            { subscribers = levelConfig.subscribers
            }
    }


setActors : Actor.LevelConfig -> Actor.Level -> Actor.Level
setActors levelConfig level =
    levelConfig.scene
        |> List.indexedMap Tuple.pair
        |> List.foldl
            (\( y, line ) accLevel ->
                String.Extra.break levelConfig.signLength line
                    |> List.indexedMap Tuple.pair
                    |> List.foldl
                        (\( x, key ) innerAccLevel ->
                            Dict.get key levelConfig.signs
                                |> Maybe.andThen (\entityName -> Dict.get entityName levelConfig.entities)
                                |> Maybe.map
                                    (\entity ->
                                        Common.addActor
                                            (Dict.insert
                                                "transform"
                                                (Actor.TransformComponent
                                                    { position = { x = x, y = y }
                                                    }
                                                )
                                                entity
                                            )
                                            innerAccLevel
                                    )
                                |> Maybe.withDefault innerAccLevel
                        )
                        accLevel
            )
            level
