module Update exposing (update, subscriptions)

import BaseType exposing (..)
import Material exposing (Material)
import Card
import Model exposing (..)
import Msg exposing (..)
import Api
import Timer
import Helper
import AnimationFrame
import Time exposing (Time)
import Debug
import Server
import Shake


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Server.listen model ServerMsgReceived
        , case timer model.stage of
            Just _ ->
                AnimationFrame.times UpdateTimer

            Nothing ->
                Sub.none
        , case model.stage of
            TradeStage _ ->
                Sub.batch
                    [ Shake.shake (always Shake)
                    , Time.every Time.second (TradeMsg << always Yield)
                    ]

            _ ->
                Sub.none
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReadyMsg msg ->
            case msg of
                Ready ->
                    ( model
                    , Server.send model Api.Ready
                    )

                NameInputChange name ->
                    ( { model | name = name }, Server.send model (Api.SetName name) )

        ProductionMsg msg ->
            tryUpdateProduction model (updateProduction msg)

        AuctionMsg msg ->
            tryUpdateAuction (updateAuction msg (Server.send model)) model

        TradeMsg msg ->
            handleTradeMsg msg model

        ServerMsgReceived action ->
            case action of
                Ok action ->
                    { model
                        | messages = toString action :: model.messages
                    }
                        |> handleAction action

                Err e ->
                    ( { model
                        | messages =
                            e :: model.messages
                      }
                    , Cmd.none
                    )

        ToggleInventory ->
            ( { model | inventoryVisible = not model.inventoryVisible }
            , Cmd.none
            )

        Shake ->
            tryUpdateTrade
                (\m ->
                    ( m, Server.send model (Api.Trade m.basket) )
                )
                model

        UpdateTimer tick ->
            ( { model | stage = updateTimer (Timer.update tick) model.stage }
            , Cmd.none
            )


updateProduction :
    ProductionMsg
    -> ProductionModel
    -> ( ProductionModel, Cmd Msg )
updateProduction msg m =
    case msg of
        FactorySelected fr ->
            ( { m | selected = Just fr }
            , Cmd.none
            )


updateAuction :
    AuctionMsg
    -> Server.SendToServer
    -> AuctionModel
    -> ( AuctionModel, Cmd Msg )
updateAuction msg send m =
    case msg of
        BidButton ->
            ( m
            , send
                (Api.Bid
                    (case m.auction of
                        Just a ->
                            Helper.nextBid a

                        Nothing ->
                            Debug.crash
                                "Bid button should be disabled when no card"
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
                identity
            )
                ( model, Cmd.none )


updateIfAuction :
    (AuctionModel -> Model -> ( Model, Cmd Msg ))
    -> Model
    -> ( Model, Cmd Msg )
updateIfAuction upd model =
    case model.stage of
        AuctionStage m ->
            upd m model

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
    (AuctionModel -> ( AuctionModel, Cmd Msg ))
    -> Model
    -> ( Model, Cmd Msg )
tryUpdateAuction upd =
    updateIfAuction <|
        \m model ->
            let
                ( newM, cmd ) =
                    upd m
            in
                ( { model | stage = AuctionStage newM }
                , cmd
                )


updateIfTrade :
    (TradeModel -> Model -> ( Model, Cmd Msg ))
    -> Model
    -> ( Model, Cmd Msg )
updateIfTrade upd model =
    case model.stage of
        TradeStage m ->
            upd m model

        _ ->
            (Debug.log
                ("Tried running update function "
                    ++ toString upd
                    ++ " during "
                    ++ toString model.stage
                )
                identity
            )
                ( model, Cmd.none )


tryUpdateTrade :
    (TradeModel -> ( TradeModel, Cmd Msg ))
    -> Model
    -> ( Model, Cmd Msg )
tryUpdateTrade upd =
    updateIfTrade
        (\m model ->
            let
                ( newM, cmd ) =
                    upd m
            in
                ( { model | stage = TradeStage newM }
                , cmd
                )
        )


handleTradeMsg : TradeMsg -> Model -> ( Model, Cmd Msg )
handleTradeMsg msg model =
    case msg of
        Yield ->
            updateIfTrade
                (\_ model ->
                    ( { model
                        | inventory =
                            Material.map
                                (\fr ->
                                    (+)
                                        (Material.lookup fr
                                            (yieldRate model.factories)
                                        )
                                )
                                model.inventory
                      }
                    , Cmd.none
                    )
                )
                model

        MoveToBasket fruit count ->
            updateIfTrade
                (\m model ->
                    case Helper.move fruit count model.inventory m.basket of
                        Nothing ->
                            Debug.crash
                                "+/- buttons should be disabled"

                        Just ( newInv, newBasket ) ->
                            tryUpdateTrade
                                (\m ->
                                    ( { m | basket = newBasket }
                                    , Cmd.none
                                    )
                                )
                                { model | inventory = newInv }
                )
                model

        EmptyBasket ->
            updateIfTrade
                (\m model ->
                    tryUpdateTrade
                        (\m ->
                            ( { m | basket = Material.empty }
                            , Cmd.none
                            )
                        )
                        { model
                            | inventory =
                                Material.map2 (always (+))
                                    m.basket
                                    model.inventory
                        }
                )
                model

        SellButton fruit ->
            case model.price of
                Just price ->
                    ( { model
                        | gold =
                            model.gold
                                + floor (Material.lookup fruit price)
                        , inventory =
                            case
                                Material.tryUpdate fruit
                                    (\x ->
                                        let
                                            newX =
                                                x - 1
                                        in
                                            if newX >= 0 then
                                                Just newX
                                            else
                                                Nothing
                                    )
                                    model.inventory
                            of
                                Nothing ->
                                    Debug.crash
                                        ("""Not enough item to sell.
                                                    Sell button should've
                                                    been disabled.""")

                                Just inv ->
                                    inv
                      }
                    , Server.send model (Api.Sell fruit 1)
                    )

                Nothing ->
                    Debug.crash
                        ("No price information."
                            ++ "Sell button should have been disabled."
                        )


baseYieldRate : number
baseYieldRate =
    1


yieldRate : Material Int -> Material Int
yieldRate =
    Material.map (always ((*) baseYieldRate))


handleAction : Api.Action -> Model -> ( Model, Cmd Msg )
handleAction action model =
    {- [todo] Finish implementing -}
    case action of
        Api.GameStateChanged stage ->
            changeStage stage model

        Api.Auction seed ->
            tryUpdateAuction
                (\m ->
                    ( { m
                        | auction =
                            Just
                                { card = Card.fromSeed seed
                                , highestBid = Nothing
                                , timer = Timer.init (5 * Time.second)
                                }
                      }
                    , Cmd.none
                    )
                )
                model

        Api.BidUpdated bid winner ->
            tryUpdateAuction
                (\m ->
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
                )
                model

        Api.Welcome ->
            ( model, Cmd.none )

        Api.SetClock ms ->
            ( { model
                | stage =
                    updateTimer
                        (Timer.setTimeLeft (toFloat ms * Time.millisecond))
                        model.stage
              }
            , Cmd.none
            )

        Api.AuctionWon ->
            {- display "You Won!" message -}
            updateIfAuction
                (\m model ->
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
                                                Debug.crash
                                                    "You won for free (???)"
                                          )
                            }

                        Nothing ->
                            model
                    , Cmd.none
                    )
                )
                model

        Api.PriceUpdated price ->
            ( { model | price = Just price }, Cmd.none )

        Api.SaleCompleted count fruit price ->
            ( { model
                | gold = model.gold + floor (price * toFloat count)
                , inventory =
                    {- [note] hides negative item error -}
                    Material.update fruit
                        (\c -> max 0 (c - count))
                        model.inventory
              }
            , Cmd.none
            )

        Api.TradeCompleted mat ->
            tryUpdateTrade
                (\m -> ( { m | basket = mat }, Cmd.none ))
                model

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
                                Material.update selected
                                    ((+) 1)
                                    model.factories

                            Nothing ->
                                model.factories

                    _ ->
                        model.factories
          }
        , cmd
        )
