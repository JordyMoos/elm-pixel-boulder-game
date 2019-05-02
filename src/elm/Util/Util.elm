module Util.Util exposing (fastConcat, fastConcatMap, lazyAll, lazyAny)


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
