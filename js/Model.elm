module Model exposing (..)

import BaseType exposing (..)
import Card exposing (Card)
import Material exposing (Fruit, Material)
import Timer exposing (Timer)
import Time exposing (Time)


type alias Model =
    { hostname : String
    , app : AppModel
    }


type AppModel
    = WelcomeScreen WelcomeModel
    | Game GameModel


type alias WelcomeModel =
    { gameNameInput : String

    {- [tmp] [hack] right now this is needed
       because we can only listen to ws
       once we know the name, otherwise
       the server will add us to a random
       game

       [note] we aren't using a bool, in case
       input changes while waiting for server
       response (though unlikely)
    -}
    , submittedName : Maybe String
    }


type alias GameModel =
    { gameName : String
    , stage : Stage
    , name : String
    , gold : Int
    , inventory : Material Int
    , factories : Material Int
    , yieldRateModifier : Material Float
    , cards : List Card
    , price : Maybe Price
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
    { hostname = hostname
    , app = initAppModel
    }


initAppModel : AppModel
initAppModel =
    WelcomeScreen initWelcomeModel


initWelcomeModel : WelcomeModel
initWelcomeModel =
    { gameNameInput = ""
    , submittedName = Nothing
    }


initGameModel : String -> GameModel
initGameModel name =
    { gameName = name
    , stage = ReadyStage initReadyModel
    , name = "Anonymous"
    , gold = 25
    , inventory = Material.empty
    , factories = Material.empty

    -- [note] should perhaps use Maybe since
    -- we are using this to represent server pushed vallue
    , yieldRateModifier = Material.create (always 1)
    , cards = []
    , price = Nothing
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
