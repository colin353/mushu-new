module Msg exposing (..)

import BaseType exposing (..)
import Time exposing (Time)
import Api


type Msg
    = ReadyMsg ReadyMsg
    | ProductionMsg ProductionMsg
    | AuctionMsg AuctionMsg
    | TradeMsg TradeMsg
    | ServerMsgReceived (Result String Api.Action)
    | ToggleInventory
    | UpdateTimer Time
    | Shake


type ReadyMsg
    = Ready Bool
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
