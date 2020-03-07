module Types exposing (Sets(..), Promos(..), SetsToChoose, Card, Cards(..))


type Sets
    = Sets (List String)


type Promos
    = Promos (List Card)


type alias SetsToChoose =
    { sets : Sets
    , promos : Promos
    }


type alias Card =
    { name : String
    , isKingdom : Bool
    , linkedCards : List String
    , coinCost : Int
    , potionCost : Bool
    , debtCost : Int
    , types : List String
    }


type Cards
    = Cards (List Card)
