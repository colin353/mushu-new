module Msg exposing (..)


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
    = None


type AuctionMsg
    = Bid
    | ClockUpdated Int
