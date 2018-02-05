module Model exposing (..)

import Msg exposing (..)
import BaseType exposing (..)
import Time exposing (Time)
import WebSocket


type alias Model =
    { stage : Stage
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


type alias TradeModel =
    { basket : Material Int
    }


initModel : String -> Model
initModel hostname =
    { stage = ReadyStage initReadyModel
    , hostname = hostname
    , gold = 25
    , inventory = emptyMaterial
    , factories = emptyMaterial
    , cards = []
    , price = Nothing
    , input = ""
    , messages = []
    , inventoryVisible = False
    }


wsURL : Model -> String
wsURL model =
    ("ws://" ++ model.hostname ++ "/join?name=Leo")


send : Model -> String -> Cmd Msg
send model =
    WebSocket.send (wsURL model)


listen : Model -> (String -> Msg) -> Sub Msg
listen model =
    WebSocket.listen (wsURL model)


initReadyModel : ReadyModel
initReadyModel =
    { ready = False }


initProductionModel : ProductionModel
initProductionModel =
    { selected = Nothing }


initTradeModel : TradeModel
initTradeModel =
    { basket = emptyMaterial }


initAuctionModel : AuctionModel
initAuctionModel =
    { auction = Nothing }


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
