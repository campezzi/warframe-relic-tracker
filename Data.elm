module Data
    exposing
        ( DropCollection
        , Era
        , Item
        , Rarity
        , Relic
        , relics
        )


type alias DropCollection =
    { c1 : Item
    , c2 : Item
    , c3 : Item
    , u1 : Item
    , u2 : Item
    , r : Item
    }


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
    , drops : DropCollection
    }


relics : List Relic
relics =
    [ Relic
        Lith
        "A2"
        (mkDrops
            "Lex Prime Barrel"
            "Forma Blueprint"
            "Valkyr Prime Blueprint"
            "Cernos Prime Blueprint"
            "Akbronco Prime Link"
            "Akstiletto Prime Blueprint"
        )
    , Relic
        Lith
        "B2"
        (mkDrops
            "Paris Prime Lower Limb"
            "Tigris Prime Stock"
            "Forma Blueprint"
            "Braton Prime Receiver"
            "Orthos Prime Blade"
            "Ballistica Prime Blueprint"
        )
    ]


mkDrops : String -> String -> String -> String -> String -> String -> DropCollection
mkDrops c1 c2 c3 u1 u2 r =
    DropCollection (Item c1) (Item c2) (Item c3) (Item u1) (Item u2) (Item r)
