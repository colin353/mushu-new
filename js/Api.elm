module Api exposing (..)

import Json.Decode as D
import Json.Encode as E


type GameStage
    = ReadyStage
    | ProductionStage
    | AuctionStage


type alias CardSeed =
    Int


type alias Price =
    { blueberry : Int
    , tomato : Int
    , corn : Int
    , purple : Int
    }


type alias Material =
    { blueberry : Int
    , tomato : Int
    , corn : Int
    , purple : Int
    }


type Action
    = GameStateChanged GameStage
    | Auction CardSeed
    | AuctionWinnerUpdated String
    | CardGranted CardSeed
    | PriceUpdated Price
    | MaterialReceived Material
    | GameOver String


decodeMessage : String -> Result String Action
decodeMessage =
    D.decodeString action


action : D.Decoder Action
action =
    D.field "action" D.string |> D.andThen actionHelp


actionHelp : String -> D.Decoder Action
actionHelp a =
    case a of
        "game_state_changed" ->
            D.map GameStateChanged <|
                (D.field "new_state" D.string
                    |> D.andThen
                        (\s ->
                            case s of
                                "ready" ->
                                    D.succeed ReadyStage

                                "production" ->
                                    D.succeed ProductionStage

                                "auction" ->
                                    D.succeed AuctionStage

                                _ ->
                                    D.fail "Unrecognized stage name"
                        )
                )

        "auction_seed" ->
            D.map Auction <|
                D.field "seed" D.int

        "auction_winner_updated" ->
            D.map AuctionWinnerUpdated <|
                D.field "winner" D.string

        "card_granted" ->
            D.map CardGranted <|
                D.field "seed" D.int

        "price_updated" ->
            D.map PriceUpdated <|
                D.field "new_prices" price

        "material_received" ->
            D.map MaterialReceived <|
                D.field "material_received" material

        "game_over" ->
            D.map GameOver <|
                D.field "winner" D.string

        _ ->
            D.fail ("Received unrecognized action from server: " ++ a)


price : D.Decoder Price
price =
    D.map4 Price
        (D.field "blueberry" D.int)
        (D.field "tomato" D.int)
        (D.field "corn" D.int)
        (D.field "purple" D.int)


material : D.Decoder Material
material =
    price


type ServerAction
    = JoinGame String
    | Ready
    | Bid Int
    | Sell Material
    | ProposeTrade Material
    | ActivateCard CardSeed


encodeToMessage : ServerAction -> String
encodeToMessage =
    E.encode 0 << encodeServerAction


encodeServerAction : ServerAction -> E.Value
encodeServerAction a =
    let
        ( actionStr, values ) =
            case a of
                JoinGame gameId ->
                    ( "join_game"
                    , [ ( "name", E.string gameId )
                      ]
                    )

                Ready ->
                    ( "ready", [] )

                Bid x ->
                    ( "bid"
                    , [ ( "amount", E.int x )
                      ]
                    )

                Sell mat ->
                    ( "sell"
                    , [ ( "material", encodeMaterial mat )
                      ]
                    )

                ProposeTrade mat ->
                    ( "propose_trade"
                    , [ ( "material", encodeMaterial mat )
                      ]
                    )

                {- [todo] Finish implementing -}
                ActivateCard seed ->
                    ( "activate_card", [] )
    in
        E.object <|
            [ ( "action", E.string actionStr )
            ]
                ++ values


encodeMaterial : Material -> E.Value
encodeMaterial { blueberry, tomato, corn, purple } =
    E.object
        [ ( "blueberry", E.int blueberry )
        , ( "tomato", E.int tomato )
        , ( "corn", E.int corn )
        , ( "purple", E.int purple )
        ]
