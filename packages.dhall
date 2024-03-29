{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
Replace the overrides' "{=}" (an empty record) with the following idea
The "//" or "⫽" means "merge these two records and
  when they have the same value, use the one on the right:"
-------------------------------
let overrides =
  { packageName =
      upstream.packageName // { updateEntity1 = "new value", updateEntity2 = "new value" }
  , packageName =
      upstream.packageName // { version = "v4.0.0" }
  , packageName =
      upstream.packageName // { repo = "https://www.example.com/path/to/new/repo.git" }
  }
-------------------------------

Example:
-------------------------------
let overrides =
  { halogen =
      upstream.halogen // { version = "master" }
  , halogen-vdom =
      upstream.halogen-vdom // { version = "v4.0.0" }
  }
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
Replace the additions' "{=}" (an empty record) with the following idea:
-------------------------------
let additions =
  { package-name =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , package-name =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , etc.
  }
-------------------------------

Example:
-------------------------------
let additions =
  { benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
  }
-------------------------------
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200423/packages.dhall
        sha256:c180a06bb5444fd950f8cbdd6605c644fd246deb397e62572b8f4a6b9dbcaf22

let overrides = {=}

let additions =
      { apexcharts =
        { dependencies =
          [ "effect"
          , "console"
          , "maybe"
          , "uuid"
          , "formatters"
          , "foreign-object"
          , "web-uievents"
          , "aff"
          , "foldable-traversable"
          , "debug"
          , "prelude"
          , "stringutils"
          , "hedwig"
          , "decimals"
          , "numbers"
          , "options"
          , "foreign"
          ]
        , repo = "https://github.com/timdeputter/purescript-apexcharts.git"
        , version = "v0.3.0"
        }
      , hedwig =
        { dependencies = [] : List Text
        , repo = "https://github.com/utkarshkukreti/purescript-hedwig.git"
        , version = "0.1.1"
        }
      , parseint =
        { dependencies = [] : List Text
        , repo = "https://github.com/athanclark/purescript-parseint.git"
        , version = "v1.1.1"
        }
      , sequence =
        { dependencies = [] : List Text
        , repo = "https://github.com/hdgarrood/purescript-sequences.git"
        , version = "v2.1.0"
        }
      , sorted-arrays =
        { dependencies = [] : List Text
        , repo = "https://github.com/vladciobanu/purescript-sorted-arrays.git"
        , version = "v0.2.0"
        }
      , web-urlsearchparams =
        { dependencies = [ "tuples-native", "iterable" ]
        , repo =
            "https://github.com/athanclark/purescript-web-urlsearchparams.git"
        , version = "v0.0.1"
        }
      , tuples-native =
        { dependencies = [ "typelevel" ]
        , repo = "https://github.com/athanclark/purescript-tuples-native.git"
        , version = "v2.0.2"
        }
      , iterable =
        { dependencies = [] : List Text
        , repo = "https://github.com/Risto-Stevcev/purescript-iterable.git"
        , version = "v2.0.0"
        }
      , halogen-rawhtml =
        { dependencies = [] : List Text
        , repo = "https://github.com/naglalakk/purescript-halogen-rawhtml.git"
        , version = "0.1.1"
        }
      , debug =
        { dependencies = [] : List Text
        , repo = "https://github.com/garyb/purescript-debug.git"
        , version = "v5.0.0"
        }
      }

in  upstream ⫽ overrides ⫽ additions
