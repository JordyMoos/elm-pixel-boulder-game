module Actor.Component.TriggerExplodableComponent exposing (updateTriggerExplodableComponent)

import Actor.Actor as Actor exposing (Actor, Level, TriggerExplodableComponentData)
import Actor.Common as Common
import Maybe.Extra
import Data.Position as Position exposing (Position)
import Data.Direction as Direction exposing (Direction)
import Actor.Cheats as Cheats
import Actor.Component.ExplodableComponent as Explodable
import Actor.Component.PhysicsComponent as Physics


updateTriggerExplodableComponent : TriggerExplodableComponentData -> Actor -> Level -> Level
updateTriggerExplodableComponent triggerData actor level =
    Common.getTransformComponent actor
        |> Maybe.Extra.toList
        |> List.concatMap
            (\transformData ->
                [ Position.addPosition transformData.position (Position.getOffsetFromDirection Direction.Left)
                , Position.addPosition transformData.position (Position.getOffsetFromDirection Direction.Up)
                , Position.addPosition transformData.position (Position.getOffsetFromDirection Direction.Right)
                , Position.addPosition transformData.position (Position.getOffsetFromDirection Direction.Down)
                ]
            )
        |> List.filterMap
            (\position ->
                Common.getActorsThatAffect position level
                    |> List.filter (willTriggerBy triggerData.triggerStrength)
                    |> List.map
                        (\explodableActor ->
                            ( position, explodableActor )
                        )
                    |> Just
            )
        |> List.concat
        |> List.foldr
            (\( position, explodableActor ) level ->
                level
                    |> Cheats.addBigExplosion position
                    |> Common.removeActorWithPosition position explodableActor.id
            )
            level


willTriggerBy : Int -> Actor -> Bool
willTriggerBy triggerStrength actor =
    [ actor ]
        |> List.filter Explodable.hasExplodableComponent
        |> List.map Physics.getPhysicsComponent
        |> Maybe.Extra.values
        |> List.filter
            (\physics ->
                physics.strength < triggerStrength
            )
        |> List.isEmpty
        |> not
