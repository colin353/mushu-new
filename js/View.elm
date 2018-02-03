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
                        Html.map ProductionMsg (productionView m)

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


inventoryView : Api.Material -> Html Msg
inventoryView mat =
    div []
        [ text ("Blueberry: " ++ toString mat.blueberry)
        , text ("Tomato: " ++ toString mat.tomato)
        , text ("Corn: " ++ toString mat.corn)
        , text ("Purple: " ++ toString mat.purple)
        ]


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


productionView : ProductionModel -> Html ProductionMsg
productionView m =
    div []
        [ button [ onClick None ] [ text "Blueberry" ]
        , button [ onClick None ] [ text "Tomato" ]
        , button [ onClick None ] [ text "Corn" ]
        , button [ onClick None ] [ text "Purple" ]
        ]


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
