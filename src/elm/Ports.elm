port module Ports exposing (keyDown, keyUp)


port keyDown : (String -> msg) -> Sub msg


port keyUp : (String -> msg) -> Sub msg
