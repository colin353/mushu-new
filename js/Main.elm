module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket
import Api
import Debug


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { stage : Stage
    , input : String
    , messages : List String
    }


type Stage
    = ReadyStage ReadyModel


type alias ReadyModel =
    { ready : Bool
    }


initReadyModel =
    { ready = False }


init : ( Model, Cmd Msg )
init =
    ( { stage = ReadyStage initReadyModel
      , input = ""
      , messages = []
      }
    , Cmd.none
    )



-- UPDATE


wsUrl =
    "ws://localhost:8080/join?name=Leo"


type Msg
    = Ready
    | Input String
    | MsgServer
    | ServerMsgReceived String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ready ->
            ( model
            , WebSocket.send wsUrl
                (Api.encodeServerAction Api.Ready)
            )

        Input newInput ->
            ( { model | input = newInput }, Cmd.none )

        MsgServer ->
            ( { model | input = "" }
            , WebSocket.send wsUrl model.input
            )

        ServerMsgReceived str ->
            case Api.decodeMessage str of
                Ok action ->
                    ( { model
                        | messages = str :: model.messages
                      }
                    , Debug.log (toString action) Cmd.none
                    )

                Err e ->
                    ( { model
                        | messages =
                            (e ++ str) :: model.messages
                      }
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen wsUrl ServerMsgReceived



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] (List.map viewMessage model.messages)
        , button [ onClick Ready ] [ text "Ready" ]
        , input [ value model.input, onInput Input ] []
        , button [ onClick MsgServer ] [ text "Send" ]
        ]


viewMessage : String -> Html msg
viewMessage msg =
    div [] [ text msg ]
