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


type Fruit
    = Blueberry
    | Tomato
    | Corn
    | Purple


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


allFruits : List Fruit
allFruits =
    [ Blueberry, Tomato, Corn, Purple ]


lookup : Fruit -> Api.Material a -> a
lookup fr mat =
    case fr of
        Blueberry ->
            mat.blueberry

        Tomato ->
            mat.tomato

        Corn ->
            mat.corn

        Purple ->
            mat.purple


updateMaterial : Fruit -> (a -> a) -> Api.Material a -> Api.Material a
updateMaterial fr upd mat =
    case fr of
        Blueberry ->
            { mat | blueberry = upd mat.blueberry }

        Tomato ->
            { mat | tomato = upd mat.tomato }

        Corn ->
            { mat | corn = upd mat.corn }

        Purple ->
            { mat | purple = upd mat.purple }


emptyMaterial : Api.Material Int
emptyMaterial =
    { blueberry = 0
    , tomato = 0
    , corn = 0
    , purple = 0
    }