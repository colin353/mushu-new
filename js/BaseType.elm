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
lookupMaterial fr =
    case fr of
        Blueberry ->
            .blueberry

        Tomato ->
            .tomato

        Corn ->
            .corn

        Purple ->
            .purple


createMaterial : (Fruit -> a) -> Material a
createMaterial f =
    { blueberry = f Blueberry
    , tomato = f Tomato
    , corn = f Corn
    , purple = f Purple
    }


mapMaterial : (Fruit -> a -> b) -> Material a -> Material b
mapMaterial f mat =
    createMaterial (\fr -> f fr (lookupMaterial fr mat))


mapMaterial2 :
    (Fruit -> a -> b -> c)
    -> Material a
    -> Material b
    -> Material c
mapMaterial2 f mat =
    mapMaterial (\fr -> f fr (lookupMaterial fr mat))


tryUpdateMaterial :
    Fruit
    -> (a -> Maybe a)
    -> Material a
    -> Maybe (Material a)
tryUpdateMaterial fruit f =
    traverseMaybe
        << mapMaterial
            (\fr ->
                if fr == fruit then
                    f
                else
                    Just
            )


traverseMaybe : Material (Maybe a) -> Maybe (Material a)
traverseMaybe mat =
    List.foldr
        (\fr ->
            Maybe.andThen
                (\m ->
                    Maybe.map
                        (\a -> updateMaterial fr (always a) m)
                        (lookupMaterial fr mat)
                )
        )
        (-- [hack] grab from Blueberry
         Maybe.map
            (\a -> createMaterial (always a))
            (lookupMaterial Blueberry mat)
        )
        allFruits


updateMaterial : Fruit -> (a -> a) -> Material a -> Material a
updateMaterial fruit upd =
    mapMaterial
        (\fr ->
            if fr == fruit then
                upd
            else
                identity
        )


emptyMaterial : Material Int
emptyMaterial =
    createMaterial (always 0)


toList : Material a -> List ( Fruit, a )
toList mat =
    List.map (\fr -> ( fr, lookupMaterial fr mat )) allFruits


foldMaterial : (Fruit -> a -> b -> b) -> b -> Material a -> b
foldMaterial acc b =
    List.foldr (uncurry acc) b << toList
