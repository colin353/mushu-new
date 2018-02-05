module View exposing (view)

import Material exposing (Material)
import Model exposing (..)
import Msg exposing (..)
import Timer
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
                        (toString
                            << floor
                            << Time.inSeconds
                            << Timer.timeLeft
                         <|
                            timer
                        )
                    ]

                Nothing ->
                    []
        ]


inventoryView : Material Int -> Html Msg
inventoryView =
    div []
        << List.singleton
        << text
        << List.foldr (++) ""
        << List.intersperse " "
        << Material.values
        << Material.map
            (\fr c -> toString c ++ Material.shorthand fr)


toolbar : Model -> Html Msg
toolbar m =
    div [] <|
        List.concat
            [ [ button [ onClick ToggleInventory ] [ text "Inventory" ] ]
            , List.map
                (\c ->
                    button
                        [ onClick (CardActivated c)
                        , disabled (Helper.isErr (Helper.tryApplyCardEffect c m))
                        ]
                    <|
                        List.map (div [] << List.singleton)
                            [ text c.name
                            , text
                                << (++) "Cost: "
                                << List.foldr (++) ""
                                << List.intersperse " "
                              <|
                                List.filterMap
                                    (\( fr, c ) ->
                                        if c /= 0 then
                                            Just <| toString c ++ Material.shorthand fr
                                        else
                                            Nothing
                                    )
                                    (Material.toList c.resourceCost)
                            ]
                )
                m.cards
            ]


readyView : ReadyModel -> Html ReadyMsg
readyView m =
    div [ class "card" ]
        [ div [ class "card-text" ] [ text "Set your name:" ]
        , input [ placeholder "Anonymous", onInput NameInputChange ] []
        , div [ class "card-text" ] [ text "Waiting for players..." ]
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
                                        << flip Material.lookup m.basket
                                    )
                                    Material.allFruits
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
                                                    == Helper.move fr
                                                        1
                                                        inventory
                                                        m.basket
                                                )
                                            ]
                                            [ text "^" ]
                                    )
                                    Material.allFruits
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
                                                    == Helper.move fr
                                                        -1
                                                        inventory
                                                        m.basket
                                                )
                                            ]
                                            [ text "v" ]
                                    )
                                    Material.allFruits
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
                                        << flip Material.lookup inventory
                                    )
                                    Material.allFruits
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
                                                    (Material.lookup fr
                                                        inventory
                                                        < 1
                                                    )
                                                ]
                                                [ text
                                                    (toString
                                                        (floor
                                                            (Material.lookup
                                                                fr
                                                                p
                                                            )
                                                        )
                                                        ++ "g"
                                                    )
                                                ]
                                        )
                                        Material.allFruits
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
                                (Material.lookup fr factories
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
            Material.allFruits


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
