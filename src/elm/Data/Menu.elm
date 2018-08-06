module Data.Menu exposing (..)

import Text


type alias ListSelector a =
    { before : List a
    , selected : a
    , after : List a
    }


type alias Menu =
    { items : ListSelector Item
    }


type alias Item =
    { key : String
    , text : Text.Letters
    }
