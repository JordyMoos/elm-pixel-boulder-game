module Util.HttpError exposing (errorToString)

import Http


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "Error: BadUrl " ++ url

        Http.Timeout ->
            "Error: Timeout"

        Http.NetworkError ->
            "Error: NetworkError"

        Http.BadStatus response ->
            "Error: BadStatus " ++ response.body

        Http.BadPayload payload response ->
            "Error: BadPayload " ++ payload ++ " \n\n\n " ++ response.body
