module Model exposing (..)

import BaseType exposing (..)
import Time exposing (Time)


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


type alias ReadyModel =
    { ready : Bool
    }


type alias ProductionModel =
    { selected : Maybe Fruit }


type alias AuctionModel =
    { auction : Maybe Auction }


type alias Auction =
    { card : Card
    , highestBid : Maybe Bid
    , timer : Timer
    }


type alias Bid =
    { bidder : String
    , bid : Int
    }


initModel : Model
initModel =
    { stage = ReadyStage initReadyModel
    , gold = 25
    , inventory = Just emptyMaterial
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
    { auction = Nothing }



-- TIMER


type Timer
    = Paused
        { lastTick : Maybe Time
        , timeLeft : Time
        }
    | Running
        { lastTick : Maybe Time
        , timeLeft : Time
        }
    | Done


startTimer : Time -> Timer
startTimer start =
    Running
        { lastTick = Nothing
        , timeLeft = start
        }


updateTimer : Time -> Timer -> Timer
updateTimer tick timer =
    case timer of
        Paused rec ->
            Paused { rec | lastTick = Just tick }

        Running rec ->
            if rec.timeLeft < 0 then
                Done
            else
                let
                    lastTick =
                        Maybe.withDefault tick rec.lastTick
                in
                    Running
                        { rec
                            | lastTick = Just tick
                            , timeLeft = rec.timeLeft - (tick - lastTick)
                        }

        Done ->
            timer


timeLeft : Timer -> Time
timeLeft timer =
    case timer of
        Paused { timeLeft } ->
            timeLeft

        Running { timeLeft } ->
            timeLeft

        Done ->
            0


resumeTimer : Timer -> Timer -> Timer
resumeTimer tick timer =
    case timer of
        Paused record ->
            Running record

        Running record ->
            Running record

        Done ->
            Done
