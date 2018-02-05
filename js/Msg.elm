module Msg exposing (..)

import BaseType exposing (..)
import Time exposing (Time)


type Msg
    = ReadyMsg ReadyMsg
    | ProductionMsg ProductionMsg
    | AuctionMsg AuctionMsg
    | TradeMsg TradeMsg
    | Input String
    | MsgServer
    | ServerMsgReceived String
    | ToggleInventory
    | AnimationFrame Time


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
