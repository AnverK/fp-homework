module BashInterpreter
    ( eval
    , Eval
    , Env (..)
    , InterpeterException (..)
    ) where

import Prelude hiding (Word, lookup)

import Control.Exception (Exception, throwIO)
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Char (isSpace)
import Data.IORef (IORef, modifyIORef, modifyIORef', readIORef)
import Data.Map (Map, fromList, insert, lookup, union)
import Data.Maybe (fromMaybe)
import Data.String (unwords)
import System.Directory (setCurrentDirectory)
import System.FilePath (normalise, (</>))

import BashParser (CommandList (..), ParamSubst (..), Parameter (..), RValue (..),
                   ShellCommand (..), Span (..), Word)

data Env = Env
  { vars    :: IORef (Map String String)
  , curPath :: IORef FilePath
  }

type Eval a = ReaderT Env IO a

data InterpeterException
  = AssignToCommandArgument
  | ExitCall
  | UnknownCommand
  deriving (Show)

instance Exception InterpeterException

wordToString :: Map String String -> Word -> String
wordToString mapVars = concatMap spanToString
  where
    spanToString :: Span -> String
    spanToString (Char c)                            = [c]
    spanToString (ParamSubst (Bare (Identifier x)))  = takeFromMap x
    spanToString (ParamSubst (Bare (CmdArgument x))) = takeFromMap x
    spanToString (Single s)                          = wordToString mapVars s
    spanToString (Double s)                          = wordToString mapVars s

    takeFromMap :: String -> String
    takeFromMap x = fromMaybe "" $ lookup x mapVars

evalAssign :: String -> Word -> Eval ()
evalAssign var value = do
  env <- ask
  mapVars <- liftIO $ readIORef $ vars env
  void $ liftIO $ modifyIORef' (vars env) (insert var $ wordToString mapVars value)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

evalEcho :: [String] -> Eval ()
evalEcho []   = liftIO $ putStrLn ""
evalEcho args@(f : rest) = if trim f == "-n" then
                             liftIO $ putStr $ unwords rest
                           else
                             liftIO $ putStrLn $ unwords args

evalRead :: [String] -> Eval ()
evalRead [] = void $ liftIO getLine
evalRead args = do
  env <- ask
  str <- liftIO getLine
  let values = splitMParts str $ length args
      kvList = zip args values
      newMap = fromList kvList
  liftIO $ modifyIORef (vars env) (union newMap)

  where
    splitMParts :: String -> Int -> [String]
    splitMParts [] _ = []
    splitMParts s 1 = [s]
    splitMParts s m = let trimmed             = dropWhile isSpace s
                          (curPart, nextPart) = break isSpace trimmed
                      in curPart : splitMParts nextPart (m - 1)

evalPwd :: [String] -> Eval ()
evalPwd _ = do
  env <- ask
  path <- liftIO $ readIORef (curPath env)
  liftIO $ putStrLn path

evalCd :: [String] -> Eval ()
evalCd [] = return ()
evalCd (path : _) = do
  env <- ask
  oldPath <- liftIO $ readIORef $ curPath env
  let newPath = normalise $ oldPath </> path

  liftIO $ setCurrentDirectory newPath
  liftIO $ modifyIORef (curPath env) (const newPath)

evalExit :: [String] -> Eval ()
evalExit _ = liftIO $ throwIO ExitCall

buildArgs :: Map String String -> Word -> String
buildArgs mapVars = concatMap splitBySpace
  where
    splitBySpace :: Span -> String
    splitBySpace (Double d) = wordToString mapVars d
    splitBySpace (Single d) = wordToString mapVars d
    splitBySpace (ParamSubst (Bare (Identifier x))) =
      removeOddSpaces $ takeFromMap x
    splitBySpace (ParamSubst (Bare (CmdArgument x))) =
      removeOddSpaces $ takeFromMap x
    splitBySpace (Char c) = [c]

    removeOddSpaces :: String -> String
    removeOddSpaces [] = []
    removeOddSpaces [c] = [c]
    removeOddSpaces s@(f : t) = let l = last t
                                    fetched = (unwords . words) s
                                in
                                if isSpace f && isSpace l then
                                  ' ' : (fetched ++ [' '])
                                else if isSpace f then
                                  ' ' : fetched
                                else if isSpace l then
                                  fetched ++ [' ']
                                else fetched

    takeFromMap :: String -> String
    takeFromMap x = fromMaybe "" $ lookup x mapVars

evalCommand :: Word -> [Word] -> Eval ()
evalCommand cmd args = do
  env <- ask
  mapVars <- liftIO $ readIORef $ vars env
  let cmdName = buildArgs mapVars cmd
      cmdArgs = filter (not . null) $ map (buildArgs mapVars) args
  case cmdName of
    "echo" -> evalEcho cmdArgs
    "read" -> evalRead cmdArgs
    "pwd"  -> evalPwd cmdArgs
    "cd"   -> evalCd cmdArgs
    "exit" -> evalExit cmdArgs
    _      -> liftIO $ throwIO UnknownCommand

evalShellCommand :: ShellCommand -> Eval ()
evalShellCommand (Assign (Identifier var) (RValue value))  = evalAssign var value
evalShellCommand (Assign _ _) = liftIO $ throwIO AssignToCommandArgument
evalShellCommand (Command [])                              = return ()
evalShellCommand (Command (cmd : args))                    = evalCommand cmd args

eval :: CommandList -> Eval ()
eval (CommandList commands) = forM_ commands evalShellCommand
