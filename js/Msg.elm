module Msg exposing (..)

import Material exposing (Fruit)
import Card exposing (Card)
import Time exposing (Time)
import Api


type Msg
    = ReadyMsg ReadyMsg
    | ProductionMsg ProductionMsg
    | AuctionMsg AuctionMsg
    | TradeMsg TradeMsg
    | ServerMsgReceived (Result String Api.Action)
    | ToggleInventory
    | CardActivated Card
    | UpdateTimer Time
    | Shake


type ReadyMsg
    = Ready
    | NameInputChange String


type TradeMsg
    = Yield
    | MoveToBasket Fruit Int
    | SellButton Fruit
    | EmptyBasket


type ProductionMsg
    = FactorySelected Fruit


type AuctionMsg
    = BidButton
    | ClockUpdated Int
