{-# LANGUAGE Rank2Types #-}

module Task7
    ( cd
    , ls
    , file
    ) where

import Prelude

import Lens.Micro (Traversal', filtered, traverseOf, traversed, (^?))

import Task6 (FS (..), anyName, dirContents, dirName, fileName)


cd :: FilePath -> Traversal' FS FS
cd nextDir = dirContents
             .traversed
             .filtered (\fs -> fs ^? dirName == Just nextDir)

ls :: Traversal' FS FilePath
ls = traverseOf (dirContents . traversed . anyName)

file :: FilePath -> Traversal' FS FilePath
file searched = dirContents
                .traversed
                .filtered (\fs -> fs ^? fileName == Just searched)
                .fileName
