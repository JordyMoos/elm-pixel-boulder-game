module Actor.Component.CounterComponetn exposing (getCounterComponent)

import Actor.Actor as Actor
    exposing
        ( Actor
        , Component(CounterComponent)
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
        (CounterComponent { counterData | counter = counterData.counter + 1 })
        actor.components
        |> Common.updateComponents actor
        |> Common.updateActor level.actors
        |> Common.updateActors level
