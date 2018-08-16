module GameState.PlayingLevel.Animation.Animation
    exposing
        ( Model
        , Action(..)
        , init
        , updateTick
        )

import Actor.Actor as Actor exposing (Level)
import Data.Position as Position exposing (Position)
import Data.Config as Config exposing (Config)
import Actor.Common as Common
import Dict
import Color
import List.Extra
import Util.PrimeSearch as PrimeSearch


type alias Model =
    { positions : List Position
    , entities : Actor.Entities
    , entityNames : List String
    }


type Action
    = Stay Model Level
    | Finished


init : List Position -> Actor.Entities -> List String -> Model
init positions entities entityNames =
    { positions = positions
    , entities = entities
    , entityNames = entityNames
    }


updateTick : Int -> Model -> Level -> Action
updateTick currentTick model level =
    case model.positions of
        [] ->
            Finished

        position :: otherPositions ->
            Stay
                { model | positions = otherPositions }
                (addActor
                    currentTick
                    model
                    (Position.addPosition position level.view.position)
                    level
                )


addActor : Int -> Model -> Position -> Level -> Level
addActor currentTick model position level =
    getEntityName currentTick model.entityNames
        |> Maybe.andThen (flip Dict.get model.entities)
        |> Maybe.map
            (\entity ->
                Common.addActor
                    (Dict.insert
                        "transform"
                        (Actor.TransformComponent { position = position, movingState = Actor.NotMoving })
                        entity
                    )
                    level
            )
        |> Maybe.withDefault level


getEntityName : Int -> List String -> Maybe String
getEntityName currentTick entityNames =
    List.Extra.getAt
        (currentTick % (List.length entityNames))
        entityNames
