module View exposing (view)

import BaseType exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Helper
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time


view : Model -> Html Msg
view model =
    div [ class "view" ] <|
        List.concat
            [ [ topBar model
              , div [ class "active-state" ]
                    [ case model.stage of
                        ReadyStage m ->
                            Html.map ReadyMsg (readyView m)

                        ProductionStage m ->
                            Html.map ProductionMsg
                                (productionView model.factories m)

                        AuctionStage m ->
                            Html.map AuctionMsg (auctionView m model.gold)

                        TradeStage m ->
                            Html.map TradeMsg (tradeView model m)
                    ]
              ]
            , [ div [ class "tray" ]
                    (List.concat
                        [ if model.inventoryVisible then
                            [ inventoryView model.inventory ]
                          else
                            []
                        , [ toolbar model
                          , div []
                                [ text ("$" ++ (toString model.gold))
                                ]
                          ]
                        ]
                    )
              , div [] (List.map viewMessage (List.reverse model.messages))
              ]
            ]


topBar : Model -> Html Msg
topBar model =
    div [ class "heading" ]
        [ div [] [ text "mushu: test" ]
        , div [] <|
            case
                timer model.stage
            of
                Just timer ->
                    [ text
                        (toString << floor << Time.inSeconds << timeLeft <| timer)
                    ]

                Nothing ->
                    []
        ]


inventoryView : Material Int -> Html Msg
inventoryView mat =
    div [] <|
        List.map
            (\fr ->
                text (toString fr ++ ": " ++ toString (lookupMaterial fr mat))
            )
            allFruits


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
    div [ class "card" ]
        [ div [ class "card-text" ] [ text "Waiting for players..." ]
        , button [ onClick Ready ] [ text "Ready" ]
        ]


tradeView : Model -> TradeModel -> Html TradeMsg
tradeView { inventory, price } m =
    let
        setDisplayStyle display =
            div << ((::) (style [ ( "display", display ) ]))

        table =
            setDisplayStyle "table"

        row =
            setDisplayStyle "table-row"

        cell =
            setDisplayStyle "table-cell"
    in
        table [] <|
            List.concat
                [ [ row [] <|
                        List.map (cell [] << List.singleton) <|
                            List.concat
                                [ [ text "Basket:" ]
                                , List.map
                                    (text
                                        << toString
                                        << flip lookupMaterial m.basket
                                    )
                                    allFruits
                                , [ button [ onClick EmptyBasket ]
                                        [ text "Empty" ]
                                  ]
                                ]
                  , row
                        []
                    <|
                        List.map (cell [] << List.singleton) <|
                            List.concat
                                [ [ text "" ]
                                , List.map
                                    (\fr ->
                                        button
                                            [ onClick (MoveToBasket fr 1)
                                            , disabled
                                                (Nothing
                                                    == move fr
                                                        1
                                                        inventory
                                                        m.basket
                                                )
                                            ]
                                            [ text "^" ]
                                    )
                                    allFruits
                                ]
                  , row
                        []
                    <|
                        List.map (cell [] << List.singleton) <|
                            List.concat
                                [ [ text "" ]
                                , List.map
                                    (\fr ->
                                        button
                                            [ onClick (MoveToBasket fr -1)
                                            , disabled
                                                (Nothing
                                                    == move fr
                                                        -1
                                                        inventory
                                                        m.basket
                                                )
                                            ]
                                            [ text "v" ]
                                    )
                                    allFruits
                                ]
                  , row
                        []
                    <|
                        List.map (cell [] << List.singleton) <|
                            List.concat
                                [ [ text "Inv:" ]
                                , List.map
                                    (text
                                        << toString
                                        << flip lookupMaterial inventory
                                    )
                                    allFruits
                                ]
                  ]
                , case price of
                    Nothing ->
                        []

                    Just p ->
                        [ row [] <|
                            List.map (cell [] << List.singleton) <|
                                List.concat
                                    [ [ text "Sell" ]
                                    , List.map
                                        (\fr ->
                                            button
                                                [ onClick (SellButton fr)
                                                , disabled
                                                    (lookupMaterial fr
                                                        inventory
                                                        < 1
                                                    )
                                                ]
                                                [ text
                                                    (toString
                                                        (floor
                                                            (lookupMaterial
                                                                fr
                                                                p
                                                            )
                                                        )
                                                        ++ "g"
                                                    )
                                                ]
                                        )
                                        allFruits
                                    ]
                        ]
                ]


productionView : Material Int -> ProductionModel -> Html ProductionMsg
productionView factories m =
    div [] <|
        List.map
            (\fr ->
                button [ onClick (FactorySelected fr) ]
                    [ text
                        (toString fr
                            ++ ": "
                            ++ toString
                                (lookupMaterial fr factories
                                    + (case m.selected of
                                        Just selected ->
                                            if selected == fr then
                                                1
                                            else
                                                0

                                        Nothing ->
                                            0
                                      )
                                )
                        )
                    ]
            )
            allFruits


auctionView : AuctionModel -> Int -> Html AuctionMsg
auctionView m gold =
    case m.auction of
        Just a ->
            div [ class "card" ] <|
                List.concat
                    [ [ div [ class "card-text" ] [ text a.card.name ] ]
                    , List.map (div [] << List.singleton) <|
                        List.concat
                            [ case a.highestBid of
                                Just { bidder, bid } ->
                                    [ text <| "Highest Bid: " ++ toString bid
                                    , text <| "Highest Bidder: " ++ bidder
                                    ]

                                Nothing ->
                                    []
                            , [ button
                                    [ onClick BidButton
                                    , disabled (gold < Helper.nextBid a)
                                    , class "card-button"
                                    ]
                                    [ text <| "Bid: " ++ toString (Helper.nextBid a) ]
                              ]
                            ]
                    ]

        Nothing ->
            text "No Cards in Auction"


viewMessage : String -> Html msg
viewMessage msg =
    div [] [ text msg ]
