module Card exposing (..)

import BaseType exposing (..)
import Material exposing (Fruit(..), Material)
import Array


type alias Card =
    { name : String
    , description : String
    , startingBid : Int
    , yieldRateModifier : Material Float
    , priceModifier : Material Float
    , resourceCost : Material Int
    , charge : Uber Int
    }


fromSeed : Int -> Card
fromSeed seed =
    case
        Array.get (seed % (List.length allCards))
            (Array.fromList allCards)
    of
        Just c ->
            c

        Nothing ->
            Debug.crash "Should never go out of bound after modding"


allCards : List Card
allCards =
    List.concat
        [ [ blueberryJam, tradeWar ]
        , Material.values famines
        , Material.values taxes
        , Material.values marketDepressions
        ]


baseCard : Card
baseCard =
    { name = "Untitled"
    , description = "No description"
    , startingBid = 3
    , yieldRateModifier = noModifier
    , priceModifier = noModifier
    , resourceCost = Material.empty
    , charge = Finite 1
    }


{-| @local
-}
blueberryJam : Card
blueberryJam =
    { baseCard
        | name = "Blueberry Jam"
        , resourceCost = Material.set Blueberry 10 Material.empty
    }


tradeWar : Card
tradeWar =
    { baseCard
        | name = "Trade War"
        , description = "When activated, the prices of all fruits will drop!"
        , priceModifier =
            {- HELP I'm trapped in an elm factory and don't know how to elm -}
            (Material.set Blueberry
                0.5
                (Material.set
                    Corn
                    0.5
                    (Material.set
                        Purple
                        0.5
                        (Material.set
                            Tomato
                            0.5
                            noModifier
                        )
                    )
                )
            )
        , resourceCost = Material.empty
    }


taxes : Material Card
taxes =
    Material.create
        (\fr ->
            { baseCard
                | name = toString fr ++ " Tax"
                , priceModifier = Material.set fr 0.8 noModifier
                , resourceCost = Material.set Blueberry 5 Material.empty
            }
        )


{-| @global
[tofix] not effects yet; server needs to push this to all players
-}
famines : Material Card
famines =
    Material.create
        (\fr ->
            { baseCard
                | name = toString fr ++ " Famine"
                , description = "When activated, the factories will yield less."
                , yieldRateModifier = Material.set fr 0.8 noModifier
                , resourceCost = Material.set Tomato 5 Material.empty
            }
        )


{-| @global
[tofix] not effects yet; server needs to push this to all players
-}
marketDepressions : Material Card
marketDepressions =
    Material.create
        (\fr ->
            { baseCard
                | name = toString fr ++ " Depression"
                , description = "When activated, demand for the fruit will drop."
                , priceModifier = Material.set fr 0.8 noModifier
                , resourceCost = Material.set Tomato 4 Material.empty
            }
        )



-- Helpers


noModifier : Material Float
noModifier =
    Material.create (always 1)
