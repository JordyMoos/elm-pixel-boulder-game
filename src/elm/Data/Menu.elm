module Data.Menu
    exposing
        ( Menu
        , ListSelector
        , Items
        , Item
        , moveMenuDown
        , moveMenuUp
        )

import Text


type alias ListSelector a =
    { before : List a
    , selected : a
    , after : List a
    }


type alias Menu =
    { items : Items
    }


type alias Items =
    ListSelector Item


type alias Item =
    { key : String
    , text : Text.Letters
    }


moveMenuDown : Menu -> Menu
moveMenuDown menu =
    moveItemsDown menu.items
        |> setItems menu


moveItemsDown : Items -> Items
moveItemsDown items =
    case items.after of
        [] ->
            items

        next :: others ->
            { before = List.append items.before [ items.selected ]
            , selected = next
            , after = others
            }


moveMenuUp : Menu -> Menu
moveMenuUp menu =
    moveItemsUp menu.items
        |> setItems menu


moveItemsUp : Items -> Items
moveItemsUp items =
    case items.before of
        [] ->
            items

        next :: others ->
            { before = others
            , selected = next
            , after = items.selected :: items.after
            }


setItems : Menu -> Items -> Menu
setItems menu items =
    { menu | items = items }
