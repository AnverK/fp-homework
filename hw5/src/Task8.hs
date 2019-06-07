{-# LANGUAGE Rank2Types #-}

module Task8
  ( changeExtension
  , getSubTree
  , removeIfEmpty
  , getPath
  , move
  ) where

import Prelude

import Lens.Micro (Traversal', filtered, traversed, (%~), (&), (^.), (^..), (^?))
import System.FilePath.Posix (replaceExtension, (</>))

import Task6 (FS (..), anyName, dirContents, dirName, fileName)

changeExtension :: String -> FS -> FS
changeExtension newExt fs = fs & (dirContents . traversed . fileName)
                               %~ (`replaceExtension` newExt)

getSubTree :: FS -> [FilePath]
getSubTree fs = fs ^. anyName : concatMap getSubTree (fs ^.. dirContents . traversed)

removeIfEmpty :: FilePath -> FS -> FS
removeIfEmpty remDir dir = dir & dirContents %~
  filter (not . (\f -> f ^? dirName == Just remDir
            && null (f ^.. dirContents . traversed)))


getPath :: Traversal' FS FilePath
getPath = anyName

move :: FilePath -> Traversal' FS FS
move nextDirName f dir =
   let Just oldDirName = dir ^? dirName
       nextDir = dir ^? dirContents
                      .traversed
                      .filtered (\n -> n ^? dirName == Just nextDirName)
   in case nextDir of
     Just (Dir _ x) -> f $ Dir (oldDirName </> nextDirName) x
     _              -> pure dir


-- testDirectory :: FS
-- testDirectory = Dir {name = "testDirectory", contents
--   = [ Dir {name = "dirB", contents = []}
--   , Dir
--       { name = "dirA"
--       , contents =
--         [ File {name = "a"}
--         , File {name = "b"}
--         ]
--       }
--   , Dir
--     { name = "dirC"
--     , contents =
--       [ File { name = "c"}
--       , File { name = "d"}
--       , Dir  { name = "dirD"
--              , contents =
--                [ File {name = "e"}
--                , Dir {name = "dirE"
--                , contents =
--                  [ File {name = "f"}]}]}]}]}
