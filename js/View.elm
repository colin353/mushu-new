module View exposing (view)

import Model exposing (..)
import Msg exposing (..)
import Api
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


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
                        Html.map AuctionMsg (auctionView m)
              ]
            , if model.inventoryVisible then
                case model.inventory of
                    Just mat ->
                        [ inventoryView mat ]

                    {- [todo] handle case -}
                    Nothing ->
                        []
              else
                []
            , [ toolbar model
              , input [ value model.input, onInput Input ] []
              , button [ onClick MsgServer ] [ text "Send" ]
              ]
            ]


inventoryView : Api.Material Int -> Html Msg
inventoryView mat =
    div [] <|
        List.map
            (\fr -> text (toString fr ++ ": " ++ toString (lookup fr mat)))
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


productionView : Api.Material Int -> ProductionModel -> Html ProductionMsg
productionView factories m =
    div [] <|
        List.map
            (\fr ->
                button [ onClick (FactorySelected fr) ]
                    [ text
                        (toString fr
                            ++ ": "
                            ++ toString
                                (lookup fr factories
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


auctionView : AuctionModel -> Html AuctionMsg
auctionView m =
    div []
        [ case m.card of
            Just c ->
                div [] <|
                    List.map (div [] << List.singleton) <|
                        [ text "Currently Bidding on:"
                        , text c.name
                        , text <|
                            case m.highBid of
                                Just x ->
                                    "Highest Bid: " ++ toString x

                                Nothing ->
                                    "No highest bid"
                        , text <|
                            case m.winner of
                                Just w ->
                                    "Highest Bidder: " ++ w

                                Nothing ->
                                    "No highest bidder"
                        , button [ onClick Bid ]
                            [ text <|
                                "Bid: "
                                    ++ toString
                                        {- [tofix] duplicate -}
                                        (case m.highBid of
                                            Just x ->
                                                x + 5

                                            Nothing ->
                                                c.startingBid
                                        )
                            ]
                        ]

            Nothing ->
                text "No Cards in Auction"
        ]


viewMessage : String -> Html msg
viewMessage msg =
    div [] [ text msg ]
