module Material
    exposing
        ( Fruit(..)
        , allFruits
        , fruitFromString
        , shorthand
        , Material
        , lookup
        , create
        , empty
        , toList
        , values
        , map
        , map2
        , traverseMaybe
        , set
        , update
        , tryUpdate
        , trySubtract
        , fold
        )


type Fruit
    = Blueberry
    | Tomato
    | Corn
    | Purple


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


shorthand : Fruit -> String
shorthand =
    String.toLower << String.left 1 << toString


type alias Material a =
    { blueberry : a
    , tomato : a
    , corn : a
    , purple : a
    }


lookup : Fruit -> Material a -> a
lookup fr =
    case fr of
        Blueberry ->
            .blueberry

        Tomato ->
            .tomato

        Corn ->
            .corn

        Purple ->
            .purple


create : (Fruit -> a) -> Material a
create f =
    { blueberry = f Blueberry
    , tomato = f Tomato
    , corn = f Corn
    , purple = f Purple
    }


empty : Material Int
empty =
    create (always 0)


toList : Material a -> List ( Fruit, a )
toList mat =
    List.map (\fr -> ( fr, lookup fr mat )) allFruits


values : Material a -> List a
values =
    toList >> List.map Tuple.second


map : (Fruit -> a -> b) -> Material a -> Material b
map f mat =
    create (\fr -> f fr (lookup fr mat))


map2 :
    (Fruit -> a -> b -> c)
    -> Material a
    -> Material b
    -> Material c
map2 f mat =
    map (\fr -> f fr (lookup fr mat))


traverseMaybe : Material (Maybe a) -> Maybe (Material a)
traverseMaybe mat =
    List.foldr
        (\fr ->
            Maybe.andThen
                (\m ->
                    Maybe.map
                        (\a -> update fr (always a) m)
                        (lookup fr mat)
                )
        )
        (-- [hack] grab from Blueberry
         Maybe.map
            (\a -> create (always a))
            (lookup Blueberry mat)
        )
        allFruits


set : Fruit -> a -> Material a -> Material a
set fruit =
    update fruit << always


update : Fruit -> (a -> a) -> Material a -> Material a
update fruit upd =
    map
        (\fr ->
            if fr == fruit then
                upd
            else
                identity
        )


tryUpdate :
    Fruit
    -> (a -> Maybe a)
    -> Material a
    -> Maybe (Material a)
tryUpdate fruit f =
    traverseMaybe
        << map
            (\fr ->
                if fr == fruit then
                    f
                else
                    Just
            )


fold : (Fruit -> a -> b -> b) -> b -> Material a -> b
fold acc b =
    List.foldr (uncurry acc) b << toList


trySubtract : Material number -> Material number -> Maybe (Material number)
trySubtract =
    curry <|
        traverseMaybe
            << uncurry
                (map2
                    (always
                        (\a b ->
                            let
                                d =
                                    b - a
                            in
                                if d >= 0 then
                                    Just d
                                else
                                    Nothing
                        )
                    )
                )
