module Actor.EventManager exposing (clearEvents, onInventoryUpdatedSubscriber, onTagDiedSubscriber)

import Actor.Actor as Actor
    exposing
        ( Event(..)
        , EventAction(..)
        , Level
        )
import Actor.Common as Common
import Dict exposing (Dict)
import Maybe.Extra


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
