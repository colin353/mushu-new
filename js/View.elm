module View exposing (view)

import Card exposing (Card)
import Material exposing (Material)
import Model exposing (..)
import Msg exposing (..)
import Timer
import Lens
import Helper
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time


view : Model -> Html Msg
view model =
    div [ class "overview" ] <|
        List.singleton <|
            Html.map AppMsg <|
                case model.app of
                    WelcomeScreen model ->
                        Html.map WelcomeMsg (welcomeView model)

                    Game model ->
                        Html.map GameMsg (gameView model)


welcomeView : WelcomeModel -> Html WelcomeMsg
welcomeView model =
    div [ class "welcome" ]
        [ div [] []
        , div [ class "box" ]
            [ div [ class "box-text" ]
                [ text "Type the name of the game to join:" ]
            , input
                [ placeholder "Game Name"
                , onInput GameNameInputChange
                ]
                []
            , button
                [ class "box-button"
                , onClick JoinGameButton
                ]
                [ text "Join Game" ]
            ]
        ]


gameView : GameModel -> Html GameMsg
gameView model =
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
                        [ [ inventoryView model.inventory ]
                        , [ toolbar model
                          , div [] []
                          ]
                        ]
                    )
              ]
            , case model.zoomCard of
                Just c ->
                    [ cardDetailView c ]

                Nothing ->
                    []
            ]


cardDetailView : Card -> Html GameMsg
cardDetailView card =
    div []
        [ div [ class "popover" ]
            [ cardView card
            , div [ class "card-activation-tray" ]
                [ div [ class "Tomato" ] [ text "Cost: 3" ]
                , button [ class "box-button" ] [ text "Activate" ]
                ]
            ]
        , div [ class "overlay", onClick (ZoomCard Nothing) ] []
        ]


icon : String -> String -> Html GameMsg
icon class_name icon_name =
    i [ class ("material-icons " ++ class_name) ] [ text icon_name ]


topBar : GameModel -> Html GameMsg
topBar model =
    div [ class "heading" ]
        [ icon "link-icon" "link"
        , div [ class "game-title" ] [ text model.gameName ]
        , div [ class "timer" ]
            (List.concat
                [ case
                    Lens.get timer model.stage
                  of
                    Just timer ->
                        [ div [ class "timer-text" ]
                            [ text
                                (toString
                                    << round
                                    << Time.inSeconds
                                    << Timer.timeLeft
                                 <|
                                    timer
                                )
                            ]
                        , icon "timer-icon" "timer"
                        ]

                    Nothing ->
                        []
                ]
            )
        ]


inventoryView : Material Int -> Html GameMsg
inventoryView inv =
    div [ class "inventory" ]
        (Material.values
            (Material.map
                (\fruit count -> div [ class ("inventory-item " ++ (toString fruit)) ] [ text (toString count) ])
                inv
            )
        )


miniCardView : Card -> Html GameMsg
miniCardView card =
    div [ onClick (ZoomCard (Just card)), class "card-micro" ] [ text card.name ]


cardPlaceholder : Html GameMsg
cardPlaceholder =
    div [ class "card-micro card-placeholder" ] []


toolbar : GameModel -> Html GameMsg
toolbar m =
    div [ class "card-shelf" ] <|
        List.take 4
            (List.concat
                [ List.map miniCardView m.cards
                , List.repeat 4 cardPlaceholder
                ]
            )


readyOrWaitingIcon : Bool -> Html ReadyMsg
readyOrWaitingIcon state =
    if state then
        i [ class ("material-icons ready-icon ready") ] [ text "check" ]
    else
        i [ class ("material-icons ready-icon waiting") ] [ text "timelapse" ]


readyView : ReadyModel -> Html ReadyMsg
readyView m =
    div []
        [ div [ class "box" ]
            [ div [ class "box-text" ] [ text "Set your name:" ]
            , input [ placeholder "Anonymous", onInput NameInputChange ] []
            , button [ class "box-button", onClick (Ready True) ] [ text "Ready" ]
            ]
        , div [ class "ready-status" ]
            [ div [ class "box-text" ] [ text "Waiting for players..." ]
            , div [ class "player-statuses" ] <|
                List.map
                    (\a ->
                        div [ class "player-status" ]
                            [ text a.name
                            , readyOrWaitingIcon a.ready
                            ]
                    )
                    m.playerInfo
            ]
        ]


tradeView : GameModel -> TradeModel -> Html TradeMsg
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


cardView : Card -> Html a
cardView card =
    div [ class "card" ]
        [ div [ class "card-heading" ]
            [ div [ class "card-title" ] [ text card.name ]
            , div [ class "card-cost" ] [ div [ class "Tomato" ] [ text "3" ] ]
            ]
        , div [ class "card-text" ] [ text "When activated, the fruit will go sour." ]
        ]


auctionView : AuctionModel -> Int -> Html AuctionMsg
auctionView m gold =
    case m.auction of
        Just a ->
            div [] <|
                List.concat
                    [ [ div [ class "box-text" ]
                            [ text "Up for auction:" ]
                      , (cardView a.card)
                      , div [ class "auction-control" ]
                            [ div [ class "auction-status" ]
                                [ div [ class "box-text" ] [ text "Winner:" ]
                                , div [ class "auction-winner" ]
                                    [ text
                                        (case a.highestBid of
                                            Just { bidder, bid } ->
                                                bidder

                                            Nothing ->
                                                "Nobody"
                                        )
                                    ]
                                ]
                            , button
                                [ onClick BidButton
                                , disabled (gold < Helper.nextBid a)
                                , class "box-button"
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
