module Update exposing (update, subscriptions)

import BaseType exposing (..)
import Material exposing (Material)
import Card exposing (Card)
import Model exposing (..)
import Msg exposing (..)
import Api
import Server
import Shake
import Timer
import Helper
import AnimationFrame
import Time exposing (Time)
import Random
import Debug


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        case model.app of
            WelcomeScreen m ->
                case m.submittedName of
                    Just gameName ->
                        [ Server.listen model
                            gameName
                            (AppMsg << ServerMsgReceived)
                        ]

                    Nothing ->
                        []

            Game m ->
                [ Server.listen model
                    m.gameName
                    (AppMsg << ServerMsgReceived)
                , case timer m.stage of
                    Just _ ->
                        AnimationFrame.times (AppMsg << GameMsg << UpdateTimer)

                    Nothing ->
                        Sub.none
                , case m.stage of
                    TradeStage _ ->
                        Sub.batch
                            [ Shake.shake
                                (AppMsg
                                    << GameMsg
                                    << TradeMsg
                                    << always Shake
                                )
                            , Time.every Time.second
                                (AppMsg
                                    << GameMsg
                                    << TradeMsg
                                    << always Yield
                                )
                            ]

                    _ ->
                        Sub.none
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AppMsg msg ->
            let
                ( m, cmd ) =
                    updateApp (Server.send model) msg model.app
            in
                ( { model | app = m }, cmd )


updateApp :
    (String -> Server.SendToServer)
    -> AppMsg
    -> AppModel
    -> ( AppModel, Cmd Msg )
updateApp toServer msg model =
    case msg of
        WelcomeMsg msg ->
            tryUpdateWelcome (updateWelcome toServer msg) model

        GameMsg msg ->
            tryUpdateGame (updateGame toServer msg) model

        ServerMsgReceived action ->
            case action of
                Ok action ->
                    model
                        |> handleAction action

                Err e ->
                    ( model
                    , Cmd.none
                    )


updateWelcome :
    (String -> Server.SendToServer)
    -> WelcomeMsg
    -> WelcomeModel
    -> ( WelcomeModel, Cmd Msg )
updateWelcome toServer msg model =
    case msg of
        JoinGameButton ->
            let
                gameName =
                    model.gameNameInput
            in
                ( { model | submittedName = Just gameName }
                , {- [question] sending Api.JoinGame even necessary?
                     or does the server add us to the game automatically
                     upon ws connection?
                  -}
                  toServer gameName (Api.JoinGame gameName)
                )

        GameNameInputChange str ->
            ( { model | gameNameInput = str }, Cmd.none )


updateGame :
    (String -> Server.SendToServer)
    -> GameMsg
    -> GameModel
    -> ( GameModel, Cmd Msg )
updateGame toServer msg model =
    let
        toGameServer =
            toServer model.gameName
    in
        case msg of
            ReadyMsg msg ->
                case msg of
                    Ready _ ->
                        ( model
                        , toGameServer (Api.Ready True)
                        )

                    NameInputChange name ->
                        ( { model | name = name }
                        , toGameServer (Api.SetName name)
                        )

            ProductionMsg msg ->
                tryUpdateProduction model (updateProduction msg)

            AuctionMsg msg ->
                tryUpdateAuction (updateAuction msg toGameServer) model

            TradeMsg msg ->
                handleTradeMsg
                    { toMsg = AppMsg << GameMsg << TradeMsg
                    }
                    toGameServer
                    msg
                    model

            ToggleInventory ->
                ( { model | inventoryVisible = not model.inventoryVisible }
                , Cmd.none
                )

            CardActivated index ->
                case
                    Helper.tryApplyCardEffect toGameServer index model
                of
                    Ok r ->
                        r

                    Err e ->
                        Debug.crash ("Card activation error: " ++ e)

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


handleTradeMsg :
    { toMsg : TradeMsg -> Msg }
    -> Server.SendToServer
    -> TradeMsg
    -> GameModel
    -> ( GameModel, Cmd Msg )
handleTradeMsg { toMsg } toServer msg model =
    case msg of
        Yield ->
            let
                roundAt : Float -> Float -> Int
                roundAt p x =
                    -- [note] only makes sense for 0 <= x <= 1
                    -- mod first to generalize?
                    if x < p then
                        ceiling x
                    else
                        floor x

                binary : Float -> Random.Generator Int
                binary p =
                    Random.float 0 1 |> Random.map (roundAt p)

                yield : Random.Generator (Material Int)
                yield =
                    let
                        matRandom =
                            Material.map2
                                (always
                                    (\p c ->
                                        binary p
                                            |> Random.list c
                                            |> Random.map List.sum
                                    )
                                )
                                model.yieldRateModifier
                                model.factories
                    in
                        Random.map4 Material
                            matRandom.blueberry
                            matRandom.tomato
                            matRandom.corn
                            matRandom.purple
            in
                ( model, Random.generate (toMsg << YieldRoll) yield )

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
                    , toServer (Api.Sell fruit 1)
                    )

                Nothing ->
                    Debug.crash
                        ("No price information."
                            ++ "Sell button should have been disabled."
                        )

        Shake ->
            tryUpdateTrade
                (\m ->
                    ( m, toServer (Api.Trade m.basket) )
                )
                model

        YieldRoll yield ->
            updateIfTrade
                (\_ model ->
                    ( { model
                        | inventory =
                            Material.map2
                                (always (+))
                                model.inventory
                                yield
                      }
                    , Cmd.none
                    )
                )
                model


handleAction : Api.Action -> AppModel -> ( AppModel, Cmd Msg )
handleAction action model =
    case action of
        Api.Welcome name ->
            ( Game (initGameModel name)
            , Cmd.none
            )

        Api.GameStateChanged stage ->
            tryUpdateGame (changeStage stage) model

        Api.Auction seed ->
            (tryUpdateGame << tryUpdateAuction)
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
            (tryUpdateGame << tryUpdateAuction)
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

        Api.SetClock ms ->
            tryUpdateGame
                (\m ->
                    ( { m
                        | stage =
                            updateTimer
                                (Timer.setTimeLeft (toFloat ms * Time.millisecond))
                                m.stage
                      }
                    , Cmd.none
                    )
                )
                model

        Api.AuctionWon ->
            {- display "You Won!" message -}
            (tryUpdateGame << updateIfAuction)
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
            tryUpdateGame
                (\m ->
                    ( { m | price = Just price }, Cmd.none )
                )
                model

        Api.EffectUpdated { yieldRateModifier } ->
            tryUpdateGame
                (\m ->
                    ( { m | yieldRateModifier = yieldRateModifier }
                    , Cmd.none
                    )
                )
                model

        Api.SaleCompleted count fruit price ->
            tryUpdateGame
                (\m ->
                    ( { m
                        | gold = m.gold + floor (price * toFloat count)
                        , inventory =
                            {- [note] hides negative item error -}
                            Material.update fruit
                                (\c -> max 0 (c - count))
                                m.inventory
                      }
                    , Cmd.none
                    )
                )
                model

        Api.TradeCompleted mat ->
            (tryUpdateGame << tryUpdateTrade)
                (\m -> ( { m | basket = mat }, Cmd.none ))
                model

        Api.GameOver winner ->
            ( model, Cmd.none )

        Api.PlayerInfoUpdated info ->
            (tryUpdateGame << tryUpdateReady)
                (\m -> ( { m | playerInfo = info }, Cmd.none ))
                model


changeStage : StageType -> GameModel -> ( GameModel, Cmd Msg )
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



-- HELPER UPDATERS


updateIfWelcome :
    (WelcomeModel -> ( AppModel, Cmd Msg ))
    -> AppModel
    -> ( AppModel, Cmd Msg )
updateIfWelcome upd model =
    case model of
        WelcomeScreen m ->
            upd m

        _ ->
            (Debug.log
                ("Tried running update function "
                    ++ toString upd
                    ++ " during "
                    ++ toString model
                )
            )
                ( model, Cmd.none )


tryUpdateWelcome :
    (WelcomeModel -> ( WelcomeModel, Cmd Msg ))
    -> AppModel
    -> ( AppModel, Cmd Msg )
tryUpdateWelcome upd =
    updateIfWelcome <|
        \m ->
            let
                ( newM, cmd ) =
                    upd m
            in
                ( WelcomeScreen newM, cmd )


updateIfGame :
    (GameModel -> ( AppModel, Cmd Msg ))
    -> AppModel
    -> ( AppModel, Cmd Msg )
updateIfGame upd model =
    case model of
        Game m ->
            upd m

        _ ->
            (Debug.log
                ("Tried running update function "
                    ++ toString upd
                    ++ " during "
                    ++ toString model
                )
            )
                ( model, Cmd.none )


tryUpdateGame :
    (GameModel -> ( GameModel, Cmd Msg ))
    -> AppModel
    -> ( AppModel, Cmd Msg )
tryUpdateGame upd =
    updateIfGame <|
        \m ->
            let
                ( newM, cmd ) =
                    upd m
            in
                ( Game newM, cmd )


updateIfReady :
    (ReadyModel -> GameModel -> ( GameModel, Cmd Msg ))
    -> GameModel
    -> ( GameModel, Cmd Msg )
updateIfReady upd model =
    case model.stage of
        ReadyStage m ->
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


tryUpdateReady :
    (ReadyModel -> ( ReadyModel, Cmd Msg ))
    -> GameModel
    -> ( GameModel, Cmd Msg )
tryUpdateReady upd =
    updateIfReady <|
        \m model ->
            let
                ( newM, cmd ) =
                    upd m
            in
                ( { model | stage = ReadyStage newM }, cmd )


tryUpdateProduction :
    GameModel
    -> (ProductionModel -> ( ProductionModel, Cmd Msg ))
    -> ( GameModel, Cmd Msg )
tryUpdateProduction model upd =
    case model.stage of
        ProductionStage m ->
            let
                ( newM, cmd ) =
                    upd m
            in
                ( { model | stage = ProductionStage newM }, cmd )

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
    (AuctionModel -> GameModel -> ( GameModel, Cmd Msg ))
    -> GameModel
    -> ( GameModel, Cmd Msg )
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
    -> GameModel
    -> ( GameModel, Cmd Msg )
tryUpdateAuction upd =
    updateIfAuction <|
        \m model ->
            let
                ( newM, cmd ) =
                    upd m
            in
                ( { model | stage = AuctionStage newM }, cmd )


updateIfTrade :
    (TradeModel -> GameModel -> ( GameModel, Cmd Msg ))
    -> GameModel
    -> ( GameModel, Cmd Msg )
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
    -> GameModel
    -> ( GameModel, Cmd Msg )
tryUpdateTrade upd =
    updateIfTrade
        (\m model ->
            let
                ( newM, cmd ) =
                    upd m
            in
                ( { model | stage = TradeStage newM }, cmd )
        )



-- HELPERS


baseYieldRate : Material Float
baseYieldRate =
    Material.create (always 1)


totalYieldRate : Material Float -> Material Int -> Material Int
totalYieldRate =
    Material.map3 (always (\a b c -> floor (a * b) * c)) baseYieldRate
