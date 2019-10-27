port module Ports exposing (keyDown, keyUp, sendText)


port keyDown : (String -> msg) -> Sub msg


port keyUp : (String -> msg) -> Sub msg


port sendText : String -> Cmd msg
