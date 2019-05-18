module Actor.Component.AreaComponent exposing (updateAreaComponent)

import Actor.Actor as Actor
    exposing
        ( Actor
        , AreaComponentData
        , Component(..)
        , Level
        , MovementComponentData
        , TransformComponentData
        )
import Actor.Common as Common
import Actor.Component.MovementComponent as MovementComponent
import Data.Position as Position
import Maybe.Extra


type alias RequiredData =
    { area : AreaComponentData
    , transform : TransformComponentData
    , movement : MovementComponentData
    }


updateAreaComponent : AreaComponentData -> Actor -> Level -> Level
updateAreaComponent area actor level =
    getRequiredData area actor
        |> Maybe.Extra.filter filterNotMoving
        |> Maybe.map (handleMovement level)
        |> Maybe.withDefault level


getRequiredData : AreaComponentData -> Actor -> Maybe RequiredData
getRequiredData area actor =
    Maybe.map3
        RequiredData
        (Just area)
        (Common.getTransformComponent actor)
        (Common.getMovementComponent actor)


filterNotMoving : RequiredData -> Bool
filterNotMoving requiredData =
    let
        result =
            MovementComponent.isNotMoving requiredData.movement

        _ =
            Debug.log "isNotMoving" (Debug.toString result)
    in
    result


handleMovement : Level -> RequiredData -> Level
handleMovement level data =
    let
        xPositions : List Int
        xPositions =
            List.range data.transform.position.x (data.transform.position.x + data.area.width)

        yPositions : List Int
        yPositions =
            List.range data.transform.position.y (data.transform.position.y + data.area.height)

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
                Common.getTransformComponent actor
                    |> Maybe.map (\transform -> MovementComponent.createMovingTowards transform.position data.area.direction data.movement)
                    |> Maybe.map (\movementData -> MovementComponent.setMovementData movementData ( actor, accLevel ))
                    |> Maybe.map Tuple.second
                    |> Maybe.map
                        (\x ->
                            let
                                newActor =
                                    Common.getActorById actor.id x

                                _ =
                                    Debug.log "moved" (Debug.toString actor.id)

                                --
                                --                                _ =
                                --                                    Debug.log "old" (Debug.toString actor)
                                --
                                _ =
                                    Debug.log "new" (Debug.toString newActor)
                            in
                            x
                        )
                    |> Maybe.withDefault accLevel
    in
    List.foldl foldY level yPositions
