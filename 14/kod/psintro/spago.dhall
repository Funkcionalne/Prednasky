{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies = [ "canvas"
                 , "console"
                 , "effect"
                 , "either"
                 , "foldable-traversable"
                 , "integers"
                 , "js-date"
                 , "math"
                 , "maybe"
                 , "partial"
                 , "prelude"
                 , "psci-support"
                 , "random"
                 , "refs"
                 , "unfoldable"
                 , "web-html"
                 ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
