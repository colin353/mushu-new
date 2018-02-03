module Msg exposing (..)

import BaseType exposing (..)


type Msg
    = ReadyMsg ReadyMsg
    | ProductionMsg ProductionMsg
    | AuctionMsg AuctionMsg
    | Input String
    | MsgServer
    | ServerMsgReceived String
    | ToggleInventory


type ReadyMsg
    = Ready


type ProductionMsg
    = FactorySelected Fruit


type AuctionMsg
    = Bid
    | ClockUpdated Int
