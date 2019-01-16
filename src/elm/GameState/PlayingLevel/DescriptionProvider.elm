module GameState.PlayingLevel.DescriptionProvider exposing (createDescription)

import Actor.Actor exposing (Level, LevelFinishedDescriptionProvider(..))
import Actor.Component.CounterComponent as CounterComponent
import Actor.Component.HealthComponent as HealthComponent
import Dict
import String


createDescription : LevelFinishedDescriptionProvider -> Level -> String
createDescription provider level =
    case provider of
        StaticDescriptionProvider description ->
            description

        AdventOfCodeDescriptionProvider ->
            adventOfCodeDescriptionProvider level


adventOfCodeDescriptionProvider : Level -> String
adventOfCodeDescriptionProvider level =
    let
        health =
            totalHealth level

        turns =
            totalTurns level
    in
    String.join " "
        [ "Health = "
        , String.fromInt health
        , ", Turns = "
        , String.fromInt turns
        , ", Total = "
        , health * turns |> String.fromInt
        ]


totalHealth : Level -> Int
totalHealth level =
    level.actors
        |> Dict.values
        |> List.filterMap HealthComponent.getHealthComponent
        |> List.map .health
        |> List.sum


totalTurns : Level -> Int
totalTurns level =
    level.actors
        |> Dict.values
        |> List.filterMap CounterComponent.getCounterComponent
        |> List.map .count
        |> List.sum
