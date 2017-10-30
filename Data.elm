module Data
    exposing
        ( Era
        , Item
        , Rarity
        , Relic
        , relics
        )


type Era
    = Lith
    | Meso
    | Neo
    | Axi


type alias Item =
    { name : String
    }


type Rarity
    = Common
    | Uncommon
    | Rare


type alias Relic =
    { era : Era
    , name : String
    , c1 : Item
    , c2 : Item
    , c3 : Item
    , u1 : Item
    , u2 : Item
    , r : Item
    }


relics : List Relic
relics =
    [ Relic
        Lith
        "A2"
        (Item "Lex Prime Barrel")
        (Item "Forma Blueprint")
        (Item "Valkyr Prime Blueprint")
        (Item "Cernos Prime Blueprint")
        (Item "Akbronco Prime Link")
        (Item "Akstiletto Prime Blueprint")
    , Relic
        Lith
        "B2"
        (Item "Paris Prime Lower Limb")
        (Item "Tigris Prime Stock")
        (Item "Forma Blueprint")
        (Item "Braton Prime Receiver")
        (Item "Orthos Prime Blade")
        (Item "Ballistica Prime Blueprint")
    ]
