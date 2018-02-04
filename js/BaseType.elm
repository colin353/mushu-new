module BaseType exposing (..)


type StageType
    = ReadyStageType
    | ProductionStageType
    | AuctionStageType
    | TradeStageType


type alias CardSeed =
    Int


type alias Material a =
    { blueberry : a
    , tomato : a
    , corn : a
    , purple : a
    }


type alias Price =
    Material Float


type Fruit
    = Blueberry
    | Tomato
    | Corn
    | Purple


type alias Card =
    { name : String
    , startingBid : Int
    }


blueberryJam : Card
blueberryJam =
    { name = "Blueberry Jam"
    , startingBid = 3
    }


allFruits : List Fruit
allFruits =
    [ Blueberry, Tomato, Corn, Purple ]


fruitFromString : String -> Maybe Fruit
fruitFromString str =
    case str of
        "blueberry" ->
            Just Blueberry

        "tomato" ->
            Just Tomato

        "corn" ->
            Just Corn

        "purple" ->
            Just Purple

        _ ->
            Nothing


lookupMaterial : Fruit -> Material a -> a
lookupMaterial fr mat =
    case fr of
        Blueberry ->
            mat.blueberry

        Tomato ->
            mat.tomato

        Corn ->
            mat.corn

        Purple ->
            mat.purple


updateMaterial : Fruit -> (a -> a) -> Material a -> Material a
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


emptyMaterial : Material Int
emptyMaterial =
    { blueberry = 0
    , tomato = 0
    , corn = 0
    , purple = 0
    }
