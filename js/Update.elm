module Update exposing (update, subscriptions)

import BaseType exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Api
import WebSocket
import AnimationFrame
import Time exposing (Time)
import Debug


wsUrl : String
wsUrl =
    "ws://localhost:8080/join?name=Leo"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.listen wsUrl ServerMsgReceived
        , AnimationFrame.times AnimationFrame
        ]


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
            tryUpdateProduction model (updateProduction msg)

        AuctionMsg msg ->
            tryUpdateAuction model (updateAuction msg)

        {- [tmp] does nothing now -}
        TradeMsg msg ->
            ( model, Cmd.none )

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

        AnimationFrame tick ->
            ( { model
                | stage =
                    case model.stage of
                        ReadyStage m ->
                            {- [todo] add timer -}
                            ReadyStage m

                        ProductionStage m ->
                            {- [todo] add timer -}
                            ProductionStage m

                        AuctionStage m ->
                            AuctionStage
                                { m
                                    | auction =
                                        Maybe.map
                                            (\a ->
                                                { a
                                                    | timer =
                                                        updateTimer tick
                                                            a.timer
                                                }
                                            )
                                            m.auction
                                }
              }
            , Cmd.none
            )


updateProduction : ProductionMsg -> ProductionModel -> ( ProductionModel, Cmd Msg )
updateProduction msg m =
    case msg of
        FactorySelected fr ->
            ( { m | selected = Just fr }
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
                        (case m.auction of
                            Just a ->
                                case a.highestBid of
                                    Just { bid } ->
                                        bid + 5

                                    Nothing ->
                                        a.card.startingBid

                            Nothing ->
                                Debug.crash
                                    "Bid button should be disabled when no card"
                        )
                    )
                )
            )

        ClockUpdated t ->
            ( m, Cmd.none )


tryUpdateProduction :
    Model
    -> (ProductionModel -> ( ProductionModel, Cmd Msg ))
    -> ( Model, Cmd Msg )
tryUpdateProduction model upd =
    case model.stage of
        ProductionStage m ->
            let
                ( newM, cmd ) =
                    upd m
            in
                ( { model | stage = ProductionStage newM }
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


updateIfAuction :
    Model
    -> (AuctionModel -> ( Model, Cmd Msg ))
    -> ( Model, Cmd Msg )
updateIfAuction model upd =
    case model.stage of
        AuctionStage m ->
            upd m

        _ ->
            (Debug.log
                ("Tried running update function "
                    ++ toString upd
                    ++ " during "
                    ++ toString model.stage
                )
            )
                ( model, Cmd.none )


tryUpdateAuction :
    Model
    -> (AuctionModel -> ( AuctionModel, Cmd Msg ))
    -> ( Model, Cmd Msg )
tryUpdateAuction model upd =
    updateIfAuction model <|
        \m ->
            let
                ( newM, cmd ) =
                    upd m
            in
                ( { model | stage = AuctionStage newM }
                , cmd
                )


handleAction : Api.Action -> Model -> ( Model, Cmd Msg )
handleAction action model =
    {- [todo] Finish implementing -}
    case action of
        Api.GameStateChanged stage ->
            changeStage stage model

        Api.Auction seed ->
            tryUpdateAuction model <|
                \m ->
                    ( { m
                        | auction =
                            Just
                                { -- [tmp] bogus card
                                  card = blueberryJam
                                , highestBid = Nothing
                                , timer = startTimer (60 * Time.second)
                                }
                      }
                    , Cmd.none
                    )

        Api.BidUpdated bid winner ->
            tryUpdateAuction model <|
                \m ->
                    ( { m
                        | auction =
                            Maybe.map
                                (\a ->
                                    { a
                                        | highestBid =
                                            Just
                                                { bidder = winner
                                                , bid = bid
                                                }
                                    }
                                )
                                m.auction
                      }
                    , Cmd.none
                    )

        Api.Welcome ->
            ( model, Cmd.none )

        Api.AuctionWon ->
            {- display "You Won!" message -}
            updateIfAuction model <|
                \m ->
                    ( case m.auction of
                        Just a ->
                            { model
                                | cards = a.card :: model.cards
                                , gold =
                                    model.gold
                                        - (case a.highestBid of
                                            Just { bid } ->
                                                bid

                                            Nothing ->
                                                Debug.crash "You won for free (???)"
                                          )
                            }

                        Nothing ->
                            model
                    , Cmd.none
                    )

        Api.PriceUpdated price ->
            ( { model | price = Just price }, Cmd.none )

        Api.SaleCompleted count fruit price ->
            ( { model
                | gold = model.gold + floor (price * toFloat count)
                , inventory =
                    {- [note] hides negative item error -}
                    Maybe.map
                        (updateMaterial fruit (\c -> max 0 (c - count)))
                        model.inventory
              }
            , Cmd.none
            )

        Api.MaterialReceived mat ->
            ( { model
                | inventory =
                    Maybe.map (addMaterial mat) model.inventory
              }
            , Cmd.none
            )

        Api.GameOver winner ->
            ( model, Cmd.none )


changeStage : StageType -> Model -> ( Model, Cmd Msg )
changeStage stage model =
    let
        ( newStage, cmd ) =
            case stage of
                ReadyStageType ->
                    ( ReadyStage initReadyModel, Cmd.none )

                ProductionStageType ->
                    ( ProductionStage initProductionModel, Cmd.none )

                AuctionStageType ->
                    ( AuctionStage initAuctionModel, Cmd.none )

                TradeStageType ->
                    ( TradeStage initTradeModel, Cmd.none )
    in
        ( { model
            | stage = newStage
            , factories =
                case model.stage of
                    ProductionStage m ->
                        case m.selected of
                            Just selected ->
                                updateMaterial selected
                                    ((+) 1)
                                    model.factories

                            Nothing ->
                                model.factories

                    _ ->
                        model.factories
          }
        , cmd
        )


addMaterial : Material Int -> Material Int -> Material Int
addMaterial m1 m2 =
    { blueberry = m1.blueberry + m2.blueberry
    , tomato = m1.tomato + m2.tomato
    , corn = m1.corn + m2.corn
    , purple = m1.purple + m2.purple
    }
