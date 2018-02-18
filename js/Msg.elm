module Msg exposing (..)

import Card exposing (Card)
import Material exposing (Fruit, Material)
import Api
import ZoomList exposing (ZoomList)
import Time exposing (Time)


type Msg
    = AppMsg AppMsg


type AppMsg
    = WelcomeMsg WelcomeMsg
    | GameMsg GameMsg
    | ServerMsgReceived (Result String Api.Action)


type WelcomeMsg
    = JoinGameButton
    | GameNameInputChange String


type GameMsg
    = ReadyMsg ReadyMsg
    | ProductionMsg ProductionMsg
    | AuctionMsg AuctionMsg
    | TradeMsg TradeMsg
    | ActivateButton
    | UpdateTimer Time
    | UpdateCards (ZoomList Card)
    | DismissCardDetailView


type ReadyMsg
    = -- [tmp] unused right now
      Ready Bool
    | NameInputChange String


type TradeMsg
    = Yield
    | MoveToBasket Fruit Int
    | SellButton Fruit
    | EmptyBasket
    | Shake
    | YieldRoll (Material Int)


type ProductionMsg
    = FactorySelected Fruit


type AuctionMsg
    = BidButton
    | ClockUpdated Int
