module Task6
    ( FS (..)
    , scanDirectory
    , anyName
    , dirContents
    , fileName
    , dirName
    , getDirName
    , getFileName
    , getSubDirs
    , changeRootName
    , addSuffixToRoot
    , getFirstDir
    , getDirFileNames
    ) where

import Prelude

import Control.Monad (guard, zipWithM)
import Data.Maybe (fromMaybe)
import Lens.Micro (Lens', Traversal', lens, traversed, (%~), (&), (.~), (^..), (^?))
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath.Posix (splitDirectories, takeFileName, (</>))
import System.IO (FilePath)

data FS
    = Dir
    { name     :: FilePath  -- название папки, не полный путь
    , contents :: [FS]
    }
    | File
    { name     :: FilePath  -- название файла, не полный путь
    }
    deriving (Show)

scanDirectory :: FilePath -> IO FS
scanDirectory root = do
  let splitted = splitDirectories root
  guard (not $ null splitted)
  scanDirectoryHelper root $ last splitted
  where
    scanDirectoryHelper :: FilePath -> FilePath -> IO FS
    scanDirectoryHelper curPath curName = do
      isFile <- doesFileExist curPath
      if isFile
        then return $ File $ takeFileName curName
        else do
          isDir <- doesDirectoryExist curPath
          guard isDir
          innerNames <- listDirectory curPath
          let innerPaths = map (curPath </>) innerNames
          innerContents <- zipWithM scanDirectoryHelper innerPaths innerNames
          return $ Dir curName innerContents

anyName :: Lens' FS FilePath
anyName = lens name (\s a -> s {name = a})

dirName :: Traversal' FS FilePath
dirName _ file@(File _) = pure file
dirName f (Dir n x)     = flip Dir x <$> f n

fileName :: Traversal' FS FilePath
fileName _ dir@(Dir _ _) = pure dir
fileName f (File n)      = File <$> f n

dirContents :: Traversal' FS [FS]
dirContents _ file@(File _) = pure file
dirContents f (Dir n x)     = Dir n <$> f x


-- Practice --

getSubDirs :: FS -> [FS]
getSubDirs fs = fs ^.. dirContents . traversed

getDirName :: FS -> Maybe FilePath
getDirName fs = fs ^? dirName

getFileName :: FS -> FilePath
getFileName fs = fromMaybe "" $ fs ^? fileName

changeRootName :: FS -> FS
changeRootName fs = fs & dirName .~ "/"

addSuffixToRoot :: String -> FS -> FS
addSuffixToRoot s fs = fs & dirName %~ (++ s)

getFirstDir :: FS -> Maybe FilePath
getFirstDir fs = fs ^? dirContents . traversed . dirName

getDirFileNames :: FS -> [FilePath]
getDirFileNames fs = fs ^.. dirContents . traversed . fileName
