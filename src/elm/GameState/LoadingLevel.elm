module GameState.LoadingLevel exposing (..)

import Data.Config exposing (Config)
import Http
import Actor
import RemoteData


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
    { config = config
    , name = name
    }
        ! [ downloadLevel name ]


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
                    Failed <| toString error

                RemoteData.Success levelConfig ->
                    Success levelConfig


downloadLevel : String -> Cmd Msg
downloadLevel name =
    Http.get
        ("./level-" ++ name ++ ".json")
        Actor.levelConfigDecoder
        |> Http.send DownloadLevelResponse
