module View exposing (view)

import BaseType exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time


bidIncrement : number
bidIncrement =
    5


view : Model -> Html Msg
view model =
    div [] <|
        List.concat
            [ [ div [] (List.map viewMessage model.messages)
              , case model.stage of
                    ReadyStage m ->
                        Html.map ReadyMsg (readyView m)

                    ProductionStage m ->
                        Html.map ProductionMsg
                            (productionView model.factories m)

                    AuctionStage m ->
                        Html.map AuctionMsg (auctionView m model.gold)

                    TradeStage m ->
                        Html.map TradeMsg (tradeView model.inventory m)
              ]
            , if model.inventoryVisible then
                [ inventoryView model.inventory ]
              else
                []
            , [ toolbar model
              , div []
                    [ text ("$" ++ (toString model.gold))
                    ]
              ]
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
    div []
        [ button [ onClick Ready ] [ text "Ready" ] ]


tradeView : Material Int -> TradeModel -> Html TradeMsg
tradeView inv m =
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
        table []
            [ row [] <|
                List.map (cell [] << List.singleton) <|
                    List.concat
                        [ [ text "Trade:" ]
                        , List.map
                            (text << toString << flip lookupMaterial m.basket)
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
                                    [ onClick (MoveToBasket fr 1)
                                    , disabled
                                        (Nothing == move fr 1 inv m.basket)
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
                                        (Nothing == move fr -1 inv m.basket)
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
                            (text << toString << flip lookupMaterial inv)
                            allFruits
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
    div []
        [ case m.auction of
            Just a ->
                div [] <|
                    List.map (div [] << List.singleton) <|
                        List.concat <|
                            [ [ text
                                    ("Time left: "
                                        ++ (toString
                                                << floor
                                                << Time.inSeconds
                                                << timeLeft
                                            <|
                                                a.timer
                                           )
                                    )
                              , text "Currently Bidding on:"
                              , text a.card.name
                              ]
                            , case a.highestBid of
                                Just { bidder, bid } ->
                                    [ text <| "Highest Bid: " ++ toString bid
                                    , text <| "Highest Bidder: " ++ bidder
                                    ]

                                Nothing ->
                                    [ text "No one bid yet" ]
                            , [ button [ onClick Msg.Bid, disabled (cantBid a.highestBid gold) ]
                                    [ text <|
                                        "Bid: "
                                            ++ toString
                                                {- [tofix] duplicate -}
                                                (case a.highestBid of
                                                    Just { bid } ->
                                                        bid + bidIncrement

                                                    Nothing ->
                                                        a.card.startingBid
                                                )
                                    ]
                              ]
                            ]

            Nothing ->
                text "No Cards in Auction"
        ]


cantBid : Maybe Bid -> Int -> Bool
cantBid bid gold =
    case bid of
        Just { bid } ->
            bid + bidIncrement > gold

        Nothing ->
            False


viewMessage : String -> Html msg
viewMessage msg =
    div [] [ text msg ]
