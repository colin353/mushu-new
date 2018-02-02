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
    | ProductionStage ProductionModel


type alias ReadyModel =
    { ready : Bool
    }


initReadyModel =
    { ready = False }


type alias ProductionModel =
    Maybe
        { blueberry : Int
        , tomato : Int
        , corn : Int
        , purple : Int
        }


initProductionModel =
    Nothing


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
    = ReadyMsg ReadyMsg
    | ProductionMsg ProductionMsg
    | Input String
    | MsgServer
    | ServerMsgReceived String


type ReadyMsg
    = Ready


type ProductionMsg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReadyMsg msg ->
            case msg of
                Ready ->
                    ( model
                    , WebSocket.send wsUrl
                        (Api.encodeToMessage Api.Ready)
                    )

        ProductionMsg msg ->
            case msg of
                None ->
                    ( model
                    , Cmd.none
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
                    { model
                        | messages = str :: model.messages
                    }
                        |> handleAction action

                Err e ->
                    ( { model
                        | messages =
                            (str ++ " <--- " ++ e) :: model.messages
                      }
                    , Cmd.none
                    )


handleAction : Api.Action -> Model -> ( Model, Cmd Msg )
handleAction action model =
    {- [todo] Finish implementing -}
    case action of
        Api.GameStateChanged stage ->
            changeStage stage model

        Api.Auction seed ->
            ( model, Cmd.none )

        Api.AuctionWinnerUpdated winner ->
            ( model, Cmd.none )

        Api.CardGranted seed ->
            ( model, Cmd.none )

        Api.PriceUpdated price ->
            ( model, Cmd.none )

        Api.MaterialReceived mat ->
            ( model, Cmd.none )

        Api.GameOver winner ->
            ( model, Cmd.none )


changeStage : Api.GameStage -> Model -> ( Model, Cmd Msg )
changeStage stage model =
    let
        ( newStage, cmd ) =
            case stage of
                Api.ReadyStage ->
                    ( ReadyStage initReadyModel, Cmd.none )

                Api.ProductionStage ->
                    ( ProductionStage initProductionModel, Cmd.none )
    in
        ( { model | stage = newStage }, cmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen wsUrl ServerMsgReceived



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] (List.map viewMessage model.messages)
        , case model.stage of
            ReadyStage m ->
                Html.map ReadyMsg (readyView m)

            ProductionStage m ->
                Html.map ProductionMsg (productionView m)
        , input [ value model.input, onInput Input ] []
        , button [ onClick MsgServer ] [ text "Send" ]
        ]


readyView : ReadyModel -> Html ReadyMsg
readyView m =
    div []
        [ button [ onClick Ready ] [ text "Ready" ] ]


productionView : ProductionModel -> Html ProductionMsg
productionView m =
    div []
        [ button [ onClick None ] [ text "Blueberry" ]
        , button [ onClick None ] [ text "Tomato" ]
        , button [ onClick None ] [ text "Corn" ]
        , button [ onClick None ] [ text "Purple" ]
        ]


viewMessage : String -> Html msg
viewMessage msg =
    div [] [ text msg ]
