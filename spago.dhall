{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "dom-simple"
, dependencies =
    [ "arrays"
    , "assert"
    , "bifunctors"
    , "console"
    , "control"
    , "effect"
    , "either"
    , "exceptions"
    , "foldable-traversable"
    , "foreign"
    , "foreign-object"
    , "identity"
    , "lists"
    , "maybe"
    , "nonempty"
    , "nullable"
    , "partial"
    , "prelude"
    , "record"
    , "transformers"
    , "typelevel-prelude"
    , "variant"
    ]
, packages = ./packages.dhall
}
