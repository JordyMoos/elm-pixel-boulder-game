module Data.Menu exposing
    ( Items
    , ListSelector
    , Menu
    , moveMenuDown
    , moveMenuUp
    )


type alias ListSelector a =
    { before : List a
    , selected : a
    , after : List a
    }


type alias Menu a =
    { items : ListSelector a
    }


type alias Items a =
    ListSelector a


moveMenuDown : Menu a -> Menu a
moveMenuDown menu =
    moveItemsDown menu.items
        |> setItems menu


moveItemsDown : Items a -> Items a
moveItemsDown items =
    case items.after of
        [] ->
            items

        next :: others ->
            { before = List.append items.before [ items.selected ]
            , selected = next
            , after = others
            }


moveMenuUp : Menu a -> Menu a
moveMenuUp menu =
    moveItemsUp menu.items
        |> setItems menu


moveItemsUp : Items a -> Items a
moveItemsUp items =
    case List.reverse items.before of
        [] ->
            items

        next :: others ->
            { before = List.reverse others
            , selected = next
            , after = items.selected :: items.after
            }


setItems : Menu a -> Items a -> Menu a
setItems menu items =
    { menu | items = items }
