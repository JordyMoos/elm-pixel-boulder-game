module Util.Util exposing (lazyAll, lazyAny)


lazyAll : List (() -> Bool) -> Bool
lazyAll =
    List.all
        (\p -> p ())


lazyAny : List (() -> Bool) -> Bool
lazyAny =
    List.any
        (\p -> p ())
