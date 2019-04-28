module Actor.Component.CounterComponent exposing (getCounterComponent, updateCounterComponent)

import Actor.Actor as Actor
    exposing
        ( Actor
        , Component(..)
        , CounterComponentData
        , Level
        )
import Actor.Common as Common
import Dict


getCounterComponent : Actor -> Maybe CounterComponentData
getCounterComponent actor =
    Dict.get "counter" actor.components
        |> Maybe.andThen
            (\component ->
                case component of
                    CounterComponent data ->
                        Just data

                    _ ->
                        Nothing
            )


updateCounterComponent : CounterComponentData -> Actor -> Level -> Level
updateCounterComponent counterData actor level =
    Dict.insert
        "counter"
        (CounterComponent { counterData | count = counterData.count + 1 })
        actor.components
        |> Common.updateComponents actor
        |> Common.updateActor level.actors
        |> Common.updateActors level
