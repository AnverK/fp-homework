module Main
    ( main
    ) where

import Prelude

import Control.Exception (catch)
import Control.Monad (guard, when)
import Data.IORef (newIORef)
import System.Environment (getArgs)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import Control.Monad.Reader (runReaderT)
import Data.Map (fromList)
import System.Directory (getCurrentDirectory)

import BashInterpreter (Env (..), eval, InterpeterException(..))
import BashParser (bashScript)

buildEnv :: [String] -> IO Env
buildEnv args = do
  initMap <- newIORef $ fromList $ zip (map (show :: Integer -> String) [0..]) args
  initPath <- getCurrentDirectory
  initPathRef <- newIORef initPath
  return $ Env initMap initPathRef

main :: IO ()
main = do
  args <- getArgs
  when (null args) $
    putStrLn "User should provide path to script in the only one argument"
  guard $ (not . null) args
  let sourcePath = head args
  content <- readFile sourcePath
  let parsed = parse bashScript sourcePath content
  case parsed of
    Left errors -> putStrLn $ errorBundlePretty errors
    Right script -> let cmdArgs = args
                    in do
                    env <- buildEnv cmdArgs
                    catch (runReaderT (eval script) env) handler

  where
    handler :: InterpeterException -> IO ()
    handler ExitCall = return ()
    handler e = print e
