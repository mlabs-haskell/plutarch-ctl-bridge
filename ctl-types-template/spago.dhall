{ name = "clarity-ctl-types"
, dependencies =
  [ "bigints"
  , "cardano-transaction-lib"
  , "maybe"
  , "newtype"
  , "prelude"
  , "profunctor-lenses"
  , "liqwid-ctl-extra"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
