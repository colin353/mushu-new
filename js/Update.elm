module Update exposing (update, subscriptions)

import Model exposing (..)
import Msg exposing (..)
import Api
import WebSocket
import Debug


wsUrl : String
wsUrl =
    "ws://localhost:8080/join?name=Leo"


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen wsUrl ServerMsgReceived


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
            ( m
            , WebSocket.send wsUrl
                (Api.encodeToMessage
                    (Api.Bid
                        {- [tofix] duplicate -}
                        (case m.card of
                            Just c ->
                                case m.highBid of
                                    Just x ->
                                        x + 5

                                    Nothing ->
                                        c.startingBid

                            Nothing ->
                                Debug.crash
                                    "Bid button should be disabled when no card"
                        )
                    )
                )
            )

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
