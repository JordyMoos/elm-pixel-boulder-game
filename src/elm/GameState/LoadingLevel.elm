module GameState.LoadingLevel exposing (Action(..), Model, Msg(..), allowedCharacters, downloadLevel, filterName, init, update, view)

import Actor.Actor as Actor
import Actor.Decoder
import Data.Config exposing (Config)
import Html exposing (Html, div, text)
import Http
import RemoteData
import Util.HttpError as HttpError


type alias Model =
    { config : Config
    , name : String
    , levelConfig : RemoteData.WebData Actor.LevelConfig
    }


type Action
    = Stay Model
    | Failed String
    | Success Actor.LevelConfig


type Msg
    = DownloadLevelResponse (RemoteData.WebData Actor.LevelConfig)


init : Config -> String -> ( Model, Cmd Msg )
init config name =
    ( { config = config
      , name = name
      , levelConfig = RemoteData.Loading
      }
    , downloadLevel name
    )


update : Msg -> Model -> Action
update msg model =
    case msg of
        DownloadLevelResponse levelConfigResponse ->
            case levelConfigResponse of
                RemoteData.NotAsked ->
                    Stay { model | levelConfig = levelConfigResponse }

                RemoteData.Loading ->
                    Stay { model | levelConfig = levelConfigResponse }

                RemoteData.Failure error ->
                    Failed <| HttpError.errorToString error

                RemoteData.Success levelConfig ->
                    Success levelConfig


view : Model -> Html Msg
view model =
    div [] [ text "Loading level..." ]


downloadLevel : String -> Cmd Msg
downloadLevel name =
    Http.get
        ("./levels/" ++ filterName name ++ ".json")
        Actor.Decoder.levelConfigDecoder
        |> RemoteData.sendRequest
        |> Cmd.map DownloadLevelResponse


filterName : String -> String
filterName name =
    name
        |> String.filter
            (String.fromChar >> (\a -> String.contains a allowedCharacters))


allowedCharacters : String
allowedCharacters =
    "abcdefghijklmnopqrstuvwxyz1234567890-/"
