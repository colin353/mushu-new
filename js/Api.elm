module Api
    exposing
        ( Action(..)
        , ServerAction(..)
        , decodeMessage
        , encodeToMessage
        )

import BaseType exposing (..)
import Json.Decode as D
import Json.Encode as E


type Action
    = GameStateChanged StageType
    | Welcome
    | Auction CardSeed
    | BidUpdated Int String
    | AuctionWon
    | PriceUpdated Price
    | SaleCompleted Int Fruit Float
    | MaterialReceived (Material Int)
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
                                    D.succeed ReadyStageType

                                "production" ->
                                    D.succeed ProductionStageType

                                "auction" ->
                                    D.succeed AuctionStageType

                                _ ->
                                    D.fail "Unrecognized stage name"
                        )
                )

        "welcome" ->
            D.succeed Welcome

        "auction_seed" ->
            D.map Auction <|
                D.field "seed" D.int

        "bid_updated" ->
            D.map2 BidUpdated
                (D.field "bid" D.int)
                (D.field "winner" D.string)

        "auction_won" ->
            D.succeed AuctionWon

        "price_updated" ->
            D.map PriceUpdated <|
                D.field "new_prices" price

        "sale_completed" ->
            D.map3 SaleCompleted
                (D.field "quantiy" D.int)
                (D.field "type" fruit)
                (D.field "price" D.float)

        "material_received" ->
            D.map MaterialReceived <|
                D.field "material_received" (material D.int)

        "game_over" ->
            D.map GameOver <|
                D.field "winner" D.string

        _ ->
            D.fail ("Received unrecognized action from server: " ++ a)


fruit : D.Decoder Fruit
fruit =
    D.string
        |> D.andThen
            (\s ->
                case fruitFromString s of
                    Just fruit ->
                        D.succeed fruit

                    Nothing ->
                        D.fail "Unrecognized fruit name"
            )


price : D.Decoder Price
price =
    material D.float


material : D.Decoder a -> D.Decoder (Material a)
material a =
    D.map4 Material
        (D.field "blueberry" a)
        (D.field "tomato" a)
        (D.field "corn" a)
        (D.field "purple" a)


type ServerAction
    = JoinGame String
    | Ready
    | Bid Int
    | Sell (Material Int)
    | ProposeTrade (Material Int)
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


encodeMaterial : Material Int -> E.Value
encodeMaterial { blueberry, tomato, corn, purple } =
    E.object
        [ ( "blueberry", E.int blueberry )
        , ( "tomato", E.int tomato )
        , ( "corn", E.int corn )
        , ( "purple", E.int purple )
        ]
