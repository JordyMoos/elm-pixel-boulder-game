module Actor.EventManager exposing (onTagDiedSubscriber, clearEvents)

import Actor.Actor as Actor
    exposing
        ( Level
        , Event(..)
        , EventAction(..)
        )
import Actor.Common as Common
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


clearEvents : Level -> Level
clearEvents level =
    { level | events = [] }
