module Util.Util exposing
    ( dictGetWithDefault
    , fastConcat
    , fastConcatMap
    , lazyAll
    , lazyAny
    )

import Dict exposing (Dict)


lazyAll : List (() -> Bool) -> Bool
lazyAll =
    List.all
        (\p -> p ())


lazyAny : List (() -> Bool) -> Bool
lazyAny =
    List.any
        (\p -> p ())


{-| Faster than List.concat

Proudly copied from <https://discourse.elm-lang.org/t/2-3x-faster-list-concat-implementation/3533>

-}
fastConcat : List (List a) -> List a
fastConcat lists =
    List.foldr (++) [] lists


fastConcatMap : (a -> List b) -> List a -> List b
fastConcatMap f list =
    fastConcat (List.map f list)


dictGetWithDefault : Dict comparable a -> comparable -> a -> a
dictGetWithDefault dict key default =
    Dict.get key dict
        |> Maybe.withDefault default
