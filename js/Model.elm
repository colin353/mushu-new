module Model exposing (..)

import BaseType exposing (..)


type alias Model =
    { stage : Stage
    , gold : Int
    , inventory : Maybe (Material Int)
    , factories : Material Int
    , cards : List Card
    , price : Maybe Price
    , input : String
    , messages : List String
    , inventoryVisible : Bool
    }


type Stage
    = ReadyStage ReadyModel
    | ProductionStage ProductionModel
    | AuctionStage AuctionModel
    | TradeStage TradeModel


type alias ReadyModel =
    { ready : Bool
    }


type alias ProductionModel =
    { selected : Maybe Fruit }


type alias AuctionModel =
    { card : Maybe Card
    , winner : Maybe String
    , highBid : Maybe Int
    , clock : Int
    }


type alias TradeModel =
    ()


initModel : Model
initModel =
    { stage = ReadyStage initReadyModel
    , gold = 0
    , inventory = Nothing
    , factories = emptyMaterial
    , cards = []
    , price = Nothing
    , input = ""
    , messages = []
    , inventoryVisible = False
    }


initReadyModel : ReadyModel
initReadyModel =
    { ready = False }


initProductionModel : ProductionModel
initProductionModel =
    { selected = Nothing }


initTradeModel : TradeModel
initTradeModel =
    ()


initAuctionModel : AuctionModel
initAuctionModel =
    { card = Nothing
    , winner = Nothing
    , highBid = Nothing
    , clock = 60

    {- [tmp] bogus value -}
    }
