module Actor.EventManager exposing (onTagDiedSubscriber)

import Actor.Actor as Actor
    exposing
        ( Level
        , Event(..)
        , EventAction(..)
        )
import Actor.Common as Common


onTagDiedSubscriber : String -> EventAction -> Event -> Level -> EventAction
onTagDiedSubscriber tag action event level =
    case event of
        ActorRemoved actor ->
            Common.getTagComponent actor
                |> Maybe.map .name
                |> Maybe.map ((==) tag)
                |> Maybe.map (always action)
                |> Maybe.withDefault (LevelContinue level)

        _ ->
            LevelContinue level
