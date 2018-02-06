module Model exposing (..)

import BaseType exposing (..)
import Time exposing (Time)
import Api


type alias Model =
    { stage : Stage
    , name : String
    , hostname : String
    , gold : Int
    , inventory : Material Int
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
    , playerInfo : List PlayerInfo
    }


type alias ProductionModel =
    { selected : Maybe Fruit
    , timer : Timer
    }


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


type alias TradeModel =
    { basket : Material Int
    , timer : Timer
    }


initModel : String -> Model
initModel hostname =
    { stage = ReadyStage initReadyModel
    , hostname = hostname
    , name = "Anonymous"
    , gold = 25
    , inventory = emptyMaterial
    , factories = emptyMaterial
    , cards = []
    , price = Nothing
    , input = ""
    , messages = []
    , inventoryVisible = False
    }


initReadyModel : ReadyModel
initReadyModel =
    { ready = False
    , playerInfo = []
    }


initProductionModel : ProductionModel
initProductionModel =
    { selected = Nothing

    -- [todo] pull this to param so it can be synced with server params
    , timer = startTimer (10 * Time.second)
    }


initTradeModel : TradeModel
initTradeModel =
    { basket = emptyMaterial
    , timer = startTimer (10 * Time.second)
    }


initAuctionModel : AuctionModel
initAuctionModel =
    { auction = Nothing }


timer : Stage -> Maybe Timer
timer stage =
    case stage of
        ReadyStage _ ->
            Nothing

        ProductionStage m ->
            Just m.timer

        AuctionStage m ->
            m.auction |> Maybe.map .timer

        TradeStage m ->
            Just m.timer


updateTimerUsing : (Timer -> Timer) -> Stage -> Stage
updateTimerUsing upd stage =
    case stage of
        ReadyStage m ->
            stage

        ProductionStage m ->
            ProductionStage { m | timer = upd m.timer }

        TradeStage m ->
            TradeStage { m | timer = upd m.timer }

        AuctionStage m ->
            AuctionStage
                { m
                    | auction = Maybe.map (\a -> { a | timer = upd a.timer }) m.auction
                }


baseYieldRate : number
baseYieldRate =
    1


yieldRate : Material Int -> Material Int
yieldRate =
    mapMaterial (always ((*) baseYieldRate))


move :
    Fruit
    -> Int
    -> Material Int
    -> Material Int
    -> Maybe ( Material Int, Material Int )
move fruit count mat1 mat2 =
    let
        newMat1 =
            updateMaterial fruit
                (flip (-) count)
                mat1

        newMat2 =
            updateMaterial fruit
                ((+) count)
                mat2
    in
        if
            lookupMaterial fruit newMat1
                < 0
                || lookupMaterial fruit newMat2
                < 0
        then
            Nothing
        else
            Just ( newMat1, newMat2 )



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


setTimeLeft : Time -> Timer -> Timer
setTimeLeft timeLeft timer =
    case timer of
        Paused rec ->
            Paused { rec | timeLeft = timeLeft }

        Running rec ->
            Running { rec | timeLeft = timeLeft }

        Done ->
            Done


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
