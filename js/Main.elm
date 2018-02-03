module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket
import Api
import Debug


main : Program Never Model Msg
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
    , cards : List Card
    , price : Maybe Api.Price
    , input : String
    , messages : List String
    , inventoryVisible : Bool
    }


type Stage
    = ReadyStage ReadyModel
    | ProductionStage ProductionModel
    | AuctionStage AuctionModel


type alias ReadyModel =
    { ready : Bool
    }


type alias ProductionModel =
    Maybe Api.Material


type alias AuctionModel =
    { card : Maybe Card
    , winner : Maybe String
    , highBid : Maybe Int
    , clock : Int
    }


type alias Card =
    { name : String
    , startingBid : Int
    }


blueberryJam : Card
blueberryJam =
    { name = "Blueberry Jam"
    , startingBid = 3
    }


initReadyModel : ReadyModel
initReadyModel =
    { ready = False }


initProductionModel : ProductionModel
initProductionModel =
    Nothing


initAuctionModel : AuctionModel
initAuctionModel =
    { card = Nothing
    , winner = Nothing
    , highBid = Nothing
    , clock = 60 {- [tmp] bogus value -}
    }


init : ( Model, Cmd Msg )
init =
    ( { stage = ReadyStage initReadyModel
      , inventory = Nothing
      , cards = []
      , price = Nothing
      , input = ""
      , messages = []
      , inventoryVisible = False
      }
    , Cmd.none
    )



-- UPDATE


wsUrl : String
wsUrl =
    "ws://localhost:8080/join?name=Leo"


type Msg
    = ReadyMsg ReadyMsg
    | ProductionMsg ProductionMsg
    | AuctionMsg AuctionMsg
    | Input String
    | MsgServer
    | ServerMsgReceived String
    | ToggleInventory


type ReadyMsg
    = Ready


type ProductionMsg
    = None


type AuctionMsg
    = Bid
    | ClockUpdated Int


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

        AuctionMsg msg ->
            tryUpdateAuction model (updateAuction msg)

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
            ( { model | inventoryVisible = not model.inventoryVisible }
            , Cmd.none
            )


updateAuction : AuctionMsg -> AuctionModel -> ( AuctionModel, Cmd Msg )
updateAuction msg m =
    case msg of
        Bid ->
            ( m, Cmd.none )

        ClockUpdated t ->
            ( m, Cmd.none )


tryUpdateAuction :
    Model
    -> (AuctionModel -> ( AuctionModel, Cmd Msg ))
    -> ( Model, Cmd Msg )
tryUpdateAuction model upd =
    case model.stage of
        AuctionStage m ->
            let
                ( newM, cmd ) =
                    upd m
            in
                ( { model | stage = AuctionStage newM }
                , cmd
                )

        _ ->
            (Debug.log
                ("Tried running update function "
                    ++ toString upd
                    ++ " during "
                    ++ toString model.stage
                )
            )
                ( model, Cmd.none )


handleAction : Api.Action -> Model -> ( Model, Cmd Msg )
handleAction action model =
    {- [todo] Finish implementing -}
    case action of
        Api.GameStateChanged stage ->
            changeStage stage model

        Api.Auction seed ->
            tryUpdateAuction model <|
                \m ->
                    ( { m | card = {- [tmp] bogus card -} Just blueberryJam }
                    , Cmd.none
                    )

        Api.AuctionWinnerUpdated winner ->
            tryUpdateAuction model <|
                \m ->
                    ( { m | winner = Just winner }, Cmd.none )

        Api.CardGranted seed ->
            let
                card =
                    {- [tmp] bogus card -}
                    blueberryJam
            in
                ( { model | cards = card :: model.cards }, Cmd.none )

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

                Api.AuctionStage ->
                    ( AuctionStage initAuctionModel, Cmd.none )
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

                    AuctionStage m ->
                        Html.map AuctionMsg (auctionView m)
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
            , [ toolbar model
              , input [ value model.input, onInput Input ] []
              , button [ onClick MsgServer ] [ text "Send" ]
              ]
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
    div [] <|
        List.concat
            [ [ button [ onClick ToggleInventory ] [ text "Inventory" ] ]
            , List.map
                (button [] << List.singleton << text << .name)
                m.cards
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


auctionView : AuctionModel -> Html AuctionMsg
auctionView m =
    div []
        [ case m.card of
            Just c ->
                div [] <|
                    List.map (div [] << List.singleton) <|
                        [ text "Currently Bidding on:"
                        , text c.name
                        , text <|
                            case m.highBid of
                                Just x ->
                                    "Highest Bid: " ++ toString x

                                Nothing ->
                                    "No highest bid"
                        , text <|
                            case m.winner of
                                Just w ->
                                    "Highest Bidder: " ++ w

                                Nothing ->
                                    "No highest bidder"
                        , button [ onClick Bid ]
                            [ text <|
                                "Bid: "
                                    ++ toString
                                        (case m.highBid of
                                            Just x ->
                                                x + 5

                                            Nothing ->
                                                c.startingBid
                                        )
                            ]
                        ]

            Nothing ->
                text "No Cards in Auction"
        ]


viewMessage : String -> Html msg
viewMessage msg =
    div [] [ text msg ]
