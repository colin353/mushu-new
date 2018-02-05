module Model exposing (..)

import BaseType exposing (..)
import Card exposing (Card)
import Material exposing (Fruit, Material)
import Timer exposing (Timer)
import Time exposing (Time)


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
    , inventory = Material.empty
    , factories = Material.empty
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
    { selected = Nothing

    -- [todo] pull this to param so it can be synced with server params
    , timer = Timer.init (10 * Time.second)
    }


initTradeModel : TradeModel
initTradeModel =
    { basket = Material.empty
    , timer = Timer.init (10 * Time.second)
    }


initAuctionModel : AuctionModel
initAuctionModel =
    { auction = Nothing }



-- GETTER & SETTERS


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


updateTimer : (Timer -> Timer) -> Stage -> Stage
updateTimer upd stage =
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
                    | auction =
                        Maybe.map
                            (\a -> { a | timer = upd a.timer })
                            m.auction
                }
