module Actor.Component.AreaComponent exposing (updateAreaComponent)

import Actor.Actor as Actor
    exposing
        ( Actor
        , AreaComponentData
        , Component(..)
        , Level
        , MovementComponentData
        , TagComponentData
        , TransformComponentData
        )
import Actor.Common as Common
import Actor.Component.MovementComponent as MovementComponent
import Data.Position as Position
import Maybe.Extra


type alias RequiredAreaData =
    { area : AreaComponentData
    , transform : TransformComponentData
    , movement : MovementComponentData
    }


type alias RequiredActorData =
    { transform : TransformComponentData
    , tag : TagComponentData
    }


updateAreaComponent : Int -> AreaComponentData -> Actor -> Level -> Level
updateAreaComponent currentTick area actor level =
    getRequiredData area actor
        |> Maybe.Extra.filter filterNotMoving
        |> Maybe.map (handleMovement currentTick level)
        |> Maybe.withDefault level


getRequiredData : AreaComponentData -> Actor -> Maybe RequiredAreaData
getRequiredData area actor =
    Maybe.map3
        RequiredAreaData
        (Just area)
        (Common.getTransformComponent actor)
        (Common.getMovementComponent actor)


filterNotMoving : RequiredAreaData -> Bool
filterNotMoving requiredData =
    MovementComponent.isNotMoving requiredData.movement


handleMovement : Int -> Level -> RequiredAreaData -> Level
handleMovement currentTick level area =
    let
        xPositions : List Int
        xPositions =
            List.range area.transform.position.x (area.transform.position.x + area.area.width)

        yPositions : List Int
        yPositions =
            List.range area.transform.position.y (area.transform.position.y + area.area.height)

        foldY : Int -> Level -> Level
        foldY =
            \y accLevel ->
                List.foldl (foldX y) accLevel xPositions

        foldX : Int -> Int -> Level -> Level
        foldX =
            \y x accLevel ->
                Common.getActorsByPosition (Position.Position x y) level
                    |> List.foldl doMovement accLevel

        doMovement : Actor -> Level -> Level
        doMovement =
            \actor accLevel ->
                Maybe.map2
                    RequiredActorData
                    (Common.getTransformComponent actor)
                    (Common.getTagComponent actor)
                    |> Maybe.Extra.filter (\actorData -> List.member actorData.tag.name area.area.tags)
                    |> Maybe.map (\actorData -> actorData.transform)
                    |> Maybe.map (\transform -> MovementComponent.createMovingTowards currentTick transform.position area.area.direction area.movement)
                    |> Maybe.map (\movementData -> MovementComponent.setMovementData movementData ( actor, accLevel ))
                    |> Maybe.map Tuple.second
                    |> Maybe.withDefault accLevel
    in
    List.foldl foldY level yPositions
