module Model exposing (..)

import Msg exposing (..)
import Api


type alias Model =
    { stage : Stage
    , inventory : Maybe Api.Material
    , cards : List Card
    , price : Maybe Api.Price
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
    Maybe Api.Material


type alias AuctionModel =
    { card : Maybe Card
    , winner : Maybe String
    , highBid : Maybe Int
    , clock : Int
    }


type alias Card =
    { name : String
    , startingBid : Int
    }


blueberryJam : Card
blueberryJam =
    { name = "Blueberry Jam"
    , startingBid = 3
    }


initReadyModel : ReadyModel
initReadyModel =
    { ready = False }


initProductionModel : ProductionModel
initProductionModel =
    Nothing


initAuctionModel : AuctionModel
initAuctionModel =
    { card = Nothing
    , winner = Nothing
    , highBid = Nothing
    , clock = 60 {- [tmp] bogus value -}
    }


init : ( Model, Cmd Msg )
init =
    ( { stage = ReadyStage initReadyModel
      , inventory = Nothing
      , cards = []
      , price = Nothing
      , input = ""
      , messages = []
      , inventoryVisible = False
      }
    , Cmd.none
    )
