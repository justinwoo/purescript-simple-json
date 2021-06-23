let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.2-20210622/packages.dhall sha256:c4949646febb03c7b1f329c9f48921c3a1e6afee133330fd24b5aa4a88112973

let addons =
  { sequences =
    { dependencies =
      [ "arrays"
      , "assert"
      , "console"
      , "effect"
      , "lazy"
      , "maybe"
      , "newtype"
      , "nonempty"
      , "partial"
      , "prelude"
      , "profunctor"
      , "psci-support"
      , "tuples"
      , "unfoldable"
      , "unsafe-coerce"
      ]
    , repo = "https://github.com/hdgarrood/purescript-sequences"
    , version = "v3.0.2"
    }
  }

in  (upstream // addons)
