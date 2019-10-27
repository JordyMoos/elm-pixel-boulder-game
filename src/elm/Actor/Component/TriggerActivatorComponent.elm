module Actor.Component.TriggerActivatorComponent exposing (updateTriggerActivatorComponent)

import Actor.Actor as Actor exposing (Actor, Level, TriggerActivatorComponentData)
import Actor.Common as Common
import Actor.Component.TriggerComponent as TriggerComponent


updateTriggerActivatorComponent : TriggerActivatorComponentData -> Actor -> Level -> Level
updateTriggerActivatorComponent triggerData actor level =
    Common.getTransformComponent actor
        |> Maybe.map
            (\{ position } ->
                Common.getActorsByPosition position level
                    |> List.filterMap (\otherActor -> TriggerComponent.getTriggerComponent otherActor)
                    |> List.foldl
                        (\triggerComponent accLevel ->
                            Common.addEvent (Actor.TriggerActivated triggerComponent) accLevel
                        )
                        level
            )
        |> Maybe.withDefault level
