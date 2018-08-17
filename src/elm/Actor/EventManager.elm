module Actor.EventManager exposing (onTagDiedSubscriber, onInventoryUpdatedSubscriber, clearEvents)

import Actor.Actor as Actor
    exposing
        ( Level
        , Event(..)
        , EventAction(..)
        )
import Actor.Common as Common
import Maybe.Extra
import Dict exposing (Dict)


onTagDiedSubscriber : String -> EventAction -> Event -> Level -> EventAction
onTagDiedSubscriber tag action event level =
    case event of
        ActorRemoved actor ->
            Common.getTagComponent actor
                |> Maybe.map .name
                |> Maybe.Extra.filter ((==) tag)
                |> Maybe.map (always action)
                |> Maybe.withDefault (LevelContinue level)

        _ ->
            LevelContinue level


onInventoryUpdatedSubscriber : String -> Int -> EventAction -> Event -> Level -> EventAction
onInventoryUpdatedSubscriber interestedIn minimumQuantity action event level =
    case event of
        InventoryUpdated inventory ->
            Dict.get interestedIn inventory
                |> Maybe.Extra.filter ((<=) minimumQuantity)
                |> Maybe.map (always action)
                |> Maybe.withDefault (LevelContinue level)

        _ ->
            LevelContinue level


clearEvents : Level -> Level
clearEvents level =
    { level | events = [] }
