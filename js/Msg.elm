module Msg exposing (..)

import BaseType exposing (..)
import Time exposing (Time)


type Msg
    = ReadyMsg ReadyMsg
    | ProductionMsg ProductionMsg
    | AuctionMsg AuctionMsg
    | Input String
    | MsgServer
    | ServerMsgReceived String
    | ToggleInventory
    | AnimationFrame Time


type ReadyMsg
    = Ready


type ProductionMsg
    = FactorySelected Fruit


type AuctionMsg
    = Bid
    | ClockUpdated Int
