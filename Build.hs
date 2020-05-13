#!/usr/bin/env stack
-- stack --install-ghc runghc --resolver lts-15 --package shake --package template --package directory --package containers --package text --package filepath --package transformers

{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -Wall          #-}

-- | Assemble README and Reflections

-- import           Control.Monad
-- import           Control.Monad.IO.Class
-- import           Data.Char
-- import           Data.Foldable
-- import           Data.List
-- import           Data.Maybe
-- import           Data.String.AnsiEscapeCodes.Strip.Text
-- import           Data.Text                              (Text)
-- import           Data.Text.Template
-- import           Data.Time
-- import           Text.Printf
-- import           Text.Read
-- import qualified Data.Set                               as S
-- import qualified Data.Text                              as T
-- import qualified Data.Text.Lazy                         as TL
-- import qualified HTMLEntities.Text                      as H

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Foldable
import           Development.Shake
import           Development.Shake.FilePath
import qualified Data.Map                   as M

opts :: ShakeOptions
opts = shakeOptions { shakeFiles     = "_build"
                    , shakeVersion   = "1.0"
                    , shakeVerbosity = Chatty
                    -- , shakeThreads   = 1    -- for benchmarks to work properly
                    }

indexFile :: FilePath
indexFile = "assets/index.html"

-- HEY: all of this is redundant, it turns out parcel handles scss and everything?

main :: IO ()
main = shakeArgs opts $ do

    action $ do
      staticFiles <- getDirectoryFiles "static" ["//*"]
      scssFiles   <- getDirectoryFiles "scss" ["//*"]
      need $
          "_site/index.html"
        : map ("_site" </>) staticFiles
       ++ map (("_site/css" </>) . (-<.> "css")) scssFiles

    "dist/app.js" %> \fp -> do
      jsFiles <- getDirectoryFiles "src" ["//*.purs","//.js"]
      need (map ("src" </>) jsFiles)
      cmd_ "spago bundle-app"
           "--main Main"
           "--to" fp

    -- parcel
    "_site/index.html" %> \_ -> do
      need [indexFile, "dist/app.js"]
      cmd "parcel build" indexFile "-d _site"

    -- sass
    "_site/css//*.css" %> \fp -> do
      let scssSrc   = "scss" </> dropDirectory1 (dropDirectory1 fp) -<.> "scss"
          staticSrc = "static" </> dropDirectory1 fp
      void . dispatchOnFilename . M.fromList $ [
          -- TODO: handle import tracking
          ( scssSrc  , \src -> cmd "sass" src fp "--style" "compressed" )
        , ( staticSrc, \src -> copyFileChanged src fp                   )
        ]

    -- static
    priority 0 $ "_site//*" %> \fp -> do
      let src = "static" </> dropDirectory1 fp
      copyFileChanged src fp


    "clean" ~> do
      removeFilesAfter "dist" ["//*"]
      removeFilesAfter "_site" ["//*"]

  -- where
    -- staticFiles = getDirectoryFiles "static" ["//*"]


    -- want ["README.md", "reflections.md", "feed.xml"]

    -- "reflections.md" %> \fp -> do
    --     days   <- getDays
    --     bodies <- forM (M.toList days) $ \(d, hasRefl) ->
    --       if hasRefl
    --         then T.pack <$> readFile' (reflOutPath d)
    --         else T.pack <$> readFile' (reflOutCodedPath d)
    --     let yearUrls   = tUnlines' . flip foldMap otherYears $ \oy ->
    --           T.pack
    --             (printf "[%04d]: https://github.com/%s/advent-of-code-%04d/blob/master/reflections.md" oy github oy)
    --                 <$ guard (oy /= year)
    --         toc = flip map (M.toList days) $ \(d, hasRefl) ->
    --           if hasRefl
    --             then printf "* [Day %d](#day-%d)" d d
    --             else printf "* [Day %d](#day-%d) *(no reflection yet)*" d d

    --         ctx = ctx0 <> M.fromList
    --           [ ("toc" , T.pack $ unlines' toc         )
    --           , ("body", T.intercalate "\n\n\n" bodies)
    --           , ("other_links", yearUrls    )
    --           ]
    --     writeTemplate fp ctx "template/reflections.md.template"

dispatchOnFilename :: M.Map FilePath (FilePath -> Action a) -> Action (Maybe a)
dispatchOnFilename = runMaybeT . asum . map (uncurry tryFp) . M.toList
  where
    tryFp fp act = do
      guard =<< lift (doesFileExist fp)
      lift $ do
        need [fp]
        act fp


