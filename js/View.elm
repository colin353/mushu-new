module View exposing (view)

import BaseType exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time


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


auctionView : AuctionModel -> Html AuctionMsg
auctionView m =
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
                            , [ button [ onClick Msg.Bid ]
                                    [ text <|
                                        "Bid: "
                                            ++ toString
                                                {- [tofix] duplicate -}
                                                (case a.highestBid of
                                                    Just { bid } ->
                                                        bid + 5

                                                    Nothing ->
                                                        a.card.startingBid
                                                )
                                    ]
                              ]
                            ]

            Nothing ->
                text "No Cards in Auction"
        ]


viewMessage : String -> Html msg
viewMessage msg =
    div [] [ text msg ]
