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
    | AnimationFrame Time
    | Shake


type ReadyMsg
    = Ready


type TradeMsg
    = Yield
    | MoveToBasket Fruit Int


type ProductionMsg
    = FactorySelected Fruit


type AuctionMsg
    = Bid
    | ClockUpdated Int
