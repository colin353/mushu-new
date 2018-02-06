module Card exposing (..)

import BaseType exposing (..)
import Material exposing (Fruit(..), Material)
import Array


type alias Card =
    { name : String
    , startingBid : Int
    , yieldRateModifier : Material Float
    , priceModifier : Material Float
    , resourceCost : Material Int
    , charge : Uber Int
    }


fromSeed : Int -> Card
fromSeed seed =
    case Array.get (seed % (List.length allCards)) (Array.fromList allCards) of
        Just c ->
            c

        Nothing ->
            Debug.crash "Should never go out of bound after modding"


allCards : List Card
allCards =
    List.concat
        [ [ blueberryJam ]
        , Material.values famines
        ]


baseCard : Card
baseCard =
    { name = "Untitled"
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


{-| @global
[tofix] not effects yet; server needs to push this to all players
-}
famines : Material Card
famines =
    Material.create
        (\fr ->
            { baseCard
                | name = toString fr ++ " Famine"
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
                , priceModifier = Material.set fr 0.8 noModifier
                , resourceCost = Material.set Tomato 4 Material.empty
            }
        )



-- Helpers


noModifier : Material Float
noModifier =
    Material.create (always 1)
