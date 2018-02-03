module Model exposing (..)

import BaseType exposing (..)


type alias Model =
    { stage : Stage
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


initModel : Model
initModel =
    { stage = ReadyStage initReadyModel
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


initAuctionModel : AuctionModel
initAuctionModel =
    { card = Nothing
    , winner = Nothing
    , highBid = Nothing
    , clock = 60 {- [tmp] bogus value -}
    }
