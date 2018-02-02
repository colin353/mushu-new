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
    , inventory : Maybe Api.Material
    , price : Maybe Api.Price
    , input : String
    , messages : List String
    , inventoryVisible : Bool
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
      , inventory = Nothing
      , price = Nothing
      , input = ""
      , messages = []
      , inventoryVisible = False
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
    | ToggleInventory


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

        ToggleInventory ->
            ( { model
                | inventoryVisible = not model.inventoryVisible
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
            ( { model | price = Just price }, Cmd.none )

        Api.MaterialReceived mat ->
            ( { model
                | inventory =
                    Maybe.map (addMaterial mat) model.inventory
              }
            , Cmd.none
            )

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


addMaterial : Api.Material -> Api.Material -> Api.Material
addMaterial m1 m2 =
    { blueberry = m1.blueberry + m2.blueberry
    , tomato = m1.tomato + m2.tomato
    , corn = m1.corn + m2.corn
    , purple = m1.purple + m2.purple
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen wsUrl ServerMsgReceived



-- VIEW


view : Model -> Html Msg
view model =
    div [] <|
        List.concat
            [ [ div [] (List.map viewMessage model.messages)
              , case model.stage of
                    ReadyStage m ->
                        Html.map ReadyMsg (readyView m)

                    ProductionStage m ->
                        Html.map ProductionMsg (productionView m)
              , input [ value model.input, onInput Input ] []
              , button [ onClick MsgServer ] [ text "Send" ]
              ]
            , if model.inventoryVisible then
                case model.inventory of
                    Just mat ->
                        [ inventoryView mat ]

                    {- [todo] handle case -}
                    Nothing ->
                        []
              else
                []
            , [ toolbar model ]
            ]


inventoryView : Api.Material -> Html Msg
inventoryView mat =
    div []
        [ text ("Blueberry: " ++ toString mat.blueberry)
        , text ("Tomato: " ++ toString mat.tomato)
        , text ("Corn: " ++ toString mat.corn)
        , text ("Purple: " ++ toString mat.purple)
        ]


toolbar : Model -> Html Msg
toolbar m =
    div []
        [ button [ onClick ToggleInventory ] [ text "Inventory" ] ]


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
