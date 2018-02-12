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


type alias Eff a =
    ( a, Cmd Msg )


type alias Upd model =
    model -> Eff model


type alias Ctx msg =
    { toServer : String -> Server.SendToServer
    , toMsg : msg -> Msg
    }


type alias GameCtx msg =
    { toGameServer : Server.SendToServer
    , toMsg : msg -> Msg
    }


update : Msg -> Upd Model
update msg model =
    case msg of
        AppMsg msg ->
            let
                ( m, cmd ) =
                    updateApp
                        { toServer = Server.send model
                        , toMsg = AppMsg
                        }
                        msg
                        model.app
            in
                ( { model | app = m }, cmd )


updateApp : Ctx AppMsg -> AppMsg -> Upd AppModel
updateApp { toServer, toMsg } msg model =
    case msg of
        WelcomeMsg msg ->
            tryUpdateL welcomeL
                (updateWelcome
                    { toServer = toServer
                    , toMsg = toMsg << WelcomeMsg
                    }
                    msg
                )
                model

        GameMsg msg ->
            tryUpdateL gameL
                (updateGame
                    { toServer = toServer
                    , toMsg = toMsg << GameMsg
                    }
                    msg
                )
                model

        ServerMsgReceived action ->
            case action of
                Ok action ->
                    model
                        |> handleAction action

                Err e ->
                    model ! []


updateWelcome : Ctx WelcomeMsg -> WelcomeMsg -> Upd WelcomeModel
updateWelcome { toServer } msg model =
    case msg of
        JoinGameButton ->
            let
                gameName =
                    model.gameNameInput
            in
                { model | submittedName = Just gameName }
                    ! [ {- [question] sending Api.JoinGame even necessary?
                           or does the server add us to the game automatically
                           upon ws connection?
                        -}
                        toServer gameName (Api.JoinGame gameName)
                      ]

        GameNameInputChange str ->
            { model | gameNameInput = str } ! []


updateGame : Ctx GameMsg -> GameMsg -> Upd GameModel
updateGame { toServer, toMsg } msg model =
    let
        toGameServer =
            toServer model.gameName
    in
        case msg of
            ReadyMsg msg ->
                case msg of
                    Ready _ ->
                        model
                            ! [ toGameServer (Api.Ready True) ]

                    NameInputChange name ->
                        { model | name = name }
                            ! [ toGameServer (Api.SetName name) ]

            ProductionMsg msg ->
                tryUpdateL productionL (updateProduction msg) model

            AuctionMsg msg ->
                tryUpdateL auctionL
                    (updateAuction
                        { toGameServer = toGameServer
                        , toMsg = toMsg << AuctionMsg
                        }
                        msg
                    )
                    model

            TradeMsg msg ->
                handleTradeMsg
                    { toGameServer = toGameServer
                    , toMsg = AppMsg << GameMsg << TradeMsg
                    }
                    msg
                    model

            ToggleInventory ->
                { model | inventoryVisible = not model.inventoryVisible } ! []

            CardActivated index ->
                case
                    Helper.tryApplyCardEffect toGameServer index model
                of
                    Ok r ->
                        r

                    Err e ->
                        Debug.crash ("Card activation error: " ++ e)

            UpdateTimer tick ->
                { model | stage = updateTimer (Timer.update tick) model.stage }
                    ! []


updateProduction : ProductionMsg -> Upd ProductionModel
updateProduction msg model =
    case msg of
        FactorySelected fr ->
            { model | selected = Just fr } ! []


updateAuction : GameCtx AuctionMsg -> AuctionMsg -> Upd AuctionModel
updateAuction { toGameServer } msg model =
    case msg of
        BidButton ->
            model
                ! [ toGameServer <|
                        Api.Bid
                            (case model.auction of
                                Just a ->
                                    Helper.nextBid a

                                Nothing ->
                                    Debug.crash
                                        ("Bid button should be "
                                            ++ "disabled when no card"
                                        )
                            )
                  ]

        ClockUpdated t ->
            model ! []


handleTradeMsg : GameCtx TradeMsg -> TradeMsg -> Upd GameModel
handleTradeMsg { toGameServer, toMsg } msg model =
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
            updateIfL tradeL
                (\m model ->
                    case Helper.move fruit count model.inventory m.basket of
                        Nothing ->
                            Debug.crash
                                "+/- buttons should be disabled"

                        Just ( newInv, newBasket ) ->
                            tryUpdateL tradeL
                                (\m -> { m | basket = newBasket } ! [])
                                { model | inventory = newInv }
                )
                model

        EmptyBasket ->
            updateIfL tradeL
                (\m model ->
                    tryUpdateL tradeL
                        (\m -> { m | basket = Material.empty } ! [])
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
                    { model
                        | gold =
                            model.gold
                                + floor (Material.lookup fruit price)
                        , inventory =
                            case
                                Material.tryUpdate
                                    fruit
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
                        ! [ toGameServer (Api.Sell fruit 1) ]

                Nothing ->
                    Debug.crash
                        ("No price information."
                            ++ "Sell button should have been disabled."
                        )

        Shake ->
            tryUpdateL tradeL
                (\m ->
                    m ! [ toGameServer (Api.Trade m.basket) ]
                )
                model

        YieldRoll yield ->
            updateIfL tradeL
                (\_ model ->
                    { model
                        | inventory =
                            Material.map2
                                (always (+))
                                model.inventory
                                yield
                    }
                        ! []
                )
                model


handleAction : Api.Action -> Upd AppModel
handleAction action model =
    case action of
        Api.Welcome name ->
            Game (initGameModel name) ! []

        Api.GameStateChanged stage ->
            tryUpdateL gameL (changeStage stage) model

        Api.Auction seed ->
            tryUpdateL (gameL |> goIn auctionL)
                (\m ->
                    { m
                        | auction =
                            Just
                                { card = Card.fromSeed seed
                                , highestBid = Nothing
                                , timer = Timer.init (5 * Time.second)
                                }
                    }
                        ! []
                )
                model

        Api.BidUpdated bid winner ->
            tryUpdateL (gameL |> goIn auctionL)
                (\m ->
                    { m
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
                        ! []
                )
                model

        Api.SetClock ms ->
            tryUpdateL gameL
                (\m ->
                    { m
                        | stage =
                            updateTimer
                                (Timer.setTimeLeft
                                    (toFloat ms * Time.millisecond)
                                )
                                m.stage
                    }
                        ! []
                )
                model

        Api.AuctionWon ->
            {- display "You Won!" message -}
            (tryUpdateL gameL << updateIfL auctionL)
                (\m model ->
                    case m.auction of
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
                                ! []

                        Nothing ->
                            model ! []
                )
                model

        Api.PriceUpdated price ->
            tryUpdateL gameL
                (\m ->
                    { m | price = Just price } ! []
                )
                model

        Api.EffectUpdated { yieldRateModifier } ->
            tryUpdateL gameL
                (\m ->
                    { m | yieldRateModifier = yieldRateModifier } ! []
                )
                model

        Api.SaleCompleted count fruit price ->
            tryUpdateL gameL
                (\m ->
                    { m
                        | gold = m.gold + floor (price * toFloat count)
                        , inventory =
                            {- [note] hides negative item error -}
                            Material.update fruit
                                (\c -> max 0 (c - count))
                                m.inventory
                    }
                        ! []
                )
                model

        Api.TradeCompleted mat ->
            tryUpdateL (gameL |> goIn tradeL)
                (\m -> { m | basket = mat } ! [])
                model

        Api.GameOver winner ->
            model ! []

        Api.PlayerInfoUpdated info ->
            tryUpdateL (gameL |> goIn readyL)
                (\m -> { m | playerInfo = info } ! [])
                model


changeStage : StageType -> Upd GameModel
changeStage stage model =
    let
        ( newStage, cmd ) =
            case stage of
                ReadyStageType ->
                    ReadyStage initReadyModel ! []

                ProductionStageType ->
                    ProductionStage initProductionModel ! []

                AuctionStageType ->
                    AuctionStage initAuctionModel ! []

                TradeStageType ->
                    TradeStage initTradeModel ! []
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


type alias Lens a b s t =
    { get : s -> Maybe a
    , set : b -> s -> t
    }


type alias EffLens a s =
    Lens a (Eff a) s (Eff s)


goIn : EffLens a b -> EffLens b c -> EffLens a c
goIn inner outer =
    { get = outer.get >> Maybe.andThen inner.get
    , set =
        \a c ->
            case outer.get c of
                Just b ->
                    outer.set (inner.set a b) c

                Nothing ->
                    Debug.log "No stage found" c ! []
    }


welcomeL : EffLens WelcomeModel AppModel
welcomeL =
    let
        get model =
            case model of
                WelcomeScreen m ->
                    Just m

                _ ->
                    Nothing

        set ( m, cmd ) model =
            ( WelcomeScreen m, cmd )
    in
        { get = get, set = set }


gameL : EffLens GameModel AppModel
gameL =
    let
        get model =
            case model of
                Game m ->
                    Just m

                _ ->
                    Nothing

        set ( m, cmd ) model =
            ( Game m, cmd )
    in
        { get = get, set = set }


readyL : EffLens ReadyModel GameModel
readyL =
    let
        get model =
            case model.stage of
                ReadyStage m ->
                    Just m

                _ ->
                    Nothing

        set ( m, cmd ) model =
            ( { model | stage = ReadyStage m }, cmd )
    in
        { get = get, set = set }


productionL : EffLens ProductionModel GameModel
productionL =
    let
        get model =
            case model.stage of
                ProductionStage m ->
                    Just m

                _ ->
                    Nothing

        set ( m, cmd ) model =
            ( { model | stage = ProductionStage m }, cmd )
    in
        { get = get, set = set }


auctionL : EffLens AuctionModel GameModel
auctionL =
    let
        get model =
            case model.stage of
                AuctionStage m ->
                    Just m

                _ ->
                    Nothing

        set ( m, cmd ) model =
            ( { model | stage = AuctionStage m }, cmd )
    in
        { get = get, set = set }


tradeL : EffLens TradeModel GameModel
tradeL =
    let
        get model =
            case model.stage of
                TradeStage m ->
                    Just m

                _ ->
                    Nothing

        set ( m, cmd ) model =
            ( { model | stage = TradeStage m }, cmd )
    in
        { get = get, set = set }


updateL : Lens a b s t -> (a -> b) -> s -> Maybe t
updateL { get, set } upd s =
    get s |> Maybe.map (\a -> set (upd a) s)


tryUpdateL : Lens a b c ( c, Cmd msg ) -> (a -> b) -> c -> ( c, Cmd msg )
tryUpdateL lens upd s =
    updateL lens upd s
        |> Maybe.withDefault (Debug.log "Cannot update" s ! [])


updateIfL : Lens a b s (Eff s) -> (a -> s -> Eff s) -> s -> Eff s
updateIfL lens upd s =
    updateIfL_ lens upd s
        |> Maybe.withDefault (Debug.log "Cannot update" s ! [])


updateIfL_ : Lens a b s t -> (a -> s -> t) -> s -> Maybe t
updateIfL_ { get } upd s =
    get s
        |> Maybe.map (flip upd s)



-- HELPERS


baseYieldRate : Material Float
baseYieldRate =
    Material.create (always 1)


totalYieldRate : Material Float -> Material Int -> Material Int
totalYieldRate =
    Material.map3 (always (\a b c -> floor (a * b) * c)) baseYieldRate
