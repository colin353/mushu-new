module Msg exposing (..)

import Model


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
    = FactorySelected Model.Fruit


type AuctionMsg
    = Bid
    | ClockUpdated Int
