module Actor.EventManager exposing
    ( clearEvents
    , onInventoryUpdatedSubscriber
    , onTagDiedSubscriber
    )

import Actor.Actor as Actor
    exposing
        ( Event(..)
        , EventAction(..)
        , InventoryUpdatedSubscriberData
        , Level
        , TagDiedSubscriberData
        )
import Actor.Common as Common
import Dict exposing (Dict)
import Maybe.Extra


onTagDiedSubscriber : EventAction -> TagDiedSubscriberData -> Event -> Level -> ( Actor.Subscriber, EventAction )
onTagDiedSubscriber onResolveEventAction data event level =
    let
        incrementCounter =
            { data | counter = data.counter + 1 }

        decideAction : TagDiedSubscriberData -> ( Actor.Subscriber, EventAction )
        decideAction newData =
            if newData.counter >= newData.limit then
                ( Actor.TagDiedSubscriber onResolveEventAction data, onResolveEventAction )

            else
                ( Actor.TagDiedSubscriber onResolveEventAction newData, LevelContinue )
    in
    case event of
        ActorRemoved actor ->
            Common.getTagComponent actor
                |> Maybe.map .name
                |> Maybe.Extra.filter ((==) data.tag)
                |> Maybe.map (always incrementCounter)
                |> Maybe.map decideAction
                |> Maybe.withDefault ( Actor.TagDiedSubscriber onResolveEventAction data, LevelContinue )

        _ ->
            ( Actor.TagDiedSubscriber onResolveEventAction data, LevelContinue )


onInventoryUpdatedSubscriber : EventAction -> InventoryUpdatedSubscriberData -> Event -> Level -> ( Actor.Subscriber, EventAction )
onInventoryUpdatedSubscriber onResolveEventAction data event level =
    case event of
        InventoryUpdated inventory ->
            Dict.get data.interestedIn inventory
                |> Maybe.Extra.filter ((<=) data.minimumQuantity)
                |> Maybe.map (always ( Actor.InventoryUpdatedSubscriber onResolveEventAction data, onResolveEventAction ))
                |> Maybe.withDefault ( Actor.InventoryUpdatedSubscriber onResolveEventAction data, LevelContinue )

        _ ->
            ( Actor.InventoryUpdatedSubscriber onResolveEventAction data, LevelContinue )


clearEvents : Level -> Level
clearEvents level =
    { level | events = [] }
