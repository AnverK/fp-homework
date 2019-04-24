module BashParser
    ( Parser
    , Word
    , Span (..)
    , Parameter (..)
    , ParamSubst (..)
    , ShellCommand (..)
    , RValue (..)
    , CommandList (..)
    , bashScript
    ) where

import Prelude hiding (Word)

import Control.Applicative (empty, liftA2, (<|>))
import Control.Monad (guard, void)
import Data.Maybe (catMaybes)
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, between, eof, many, noneOf, notFollowedBy, oneOf, try)
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space, string)
import qualified Text.Megaparsec.Char.Lexer as L (decimal, lexeme, symbol)

type Parser = Parsec Void String

type Word = [Span]

data Span
    = Char Char
    | Single Word
    | Double Word
    | ParamSubst ParamSubst
    deriving (Show)

data Parameter
  = Identifier String
  | CmdArgument String
  deriving (Show)

newtype ParamSubst = Bare Parameter
    deriving (Show)

newtype RValue = RValue Word deriving (Show)

data ShellCommand
  = Assign Parameter RValue
  | Command [Word]
  -- | If CommandList CommandList (Maybe CommandList)
  -- | While CommandList CommandList
  deriving (Show)

newtype CommandList = CommandList [ShellCommand] deriving (Show)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

inlineSpace :: Parser Char
inlineSpace = oneOf " \t"

symbol :: String -> Parser String
symbol = L.symbol space

spans :: String             -- ^ Delimiters
      -> Parser Span        -- ^ Inner spans
      -> Parser Word
spans delims innerSpan = catMaybes <$> many inner
  where
    inner = Nothing     <$  escapedNewline
        <|> Just        <$> innerSpan
        <|> Just . Char <$> noneOf delims

    escapedNewline = try (string "\\\n")

dollar :: Parser Span
dollar = char '$' *> rest
  where
    rest = bareSubst
       <|> pure (Char '$')

    bareSubst = ParamSubst . Bare <$> bareParam
      where
        bareParam :: Parser Parameter
        bareParam = Identifier <$> try name
                    <|> CmdArgument . (show :: Integer -> String) <$>
                          (L.decimal
                      <|> between (char '{') (char '}') L.decimal)

name :: Parser String
name = (:) <$> nameStart <*> many nameLetter
 where
   nameStart  = letterChar   <|> char '_'
   nameLetter = alphaNumChar <|> char '_'

singleQuote :: Parser Span
singleQuote = Single <$> between (char '\'') (char '\'') (spans "\'" empty)

doubleQuote :: Parser Span
doubleQuote = Double <$> between (char '\"') (char '\"') (spans "\"" inner)
  where
    inner = try (Char <$ char '\\' <*> oneOf "$\\`\"")
        <|> dollar

wordSpan :: Parser Span
wordSpan = try (Char <$ char '\\' <*> anySingle)
       <|> singleQuote
       <|> doubleQuote
       <|> dollar

word :: Parser Word
word = spans " \t\r\n;()" wordSpan

assign :: Parser ShellCommand
assign = do
  var <- Identifier <$> name
  void (char '=')
  Assign var <$> (RValue <$> word)

reservedWordsList :: [String]
reservedWordsList = [ "!", "[[", "]]", "{", "}"
    , "if", "then", "else", "elif", "fi"
    , "case", "esac", "for", "select", "while", "until"
    , "in", "do", "done", "time", "function"
    ]

reservedWord :: Parser Word
reservedWord = foldr ((<|>) . rword) empty reservedWordsList
  where
    rword :: String -> Parser Word
    rword s = do
      _ <- string s
      notFollowedBy alphaNumChar
      return $ map Char s

nonReservedWord :: Parser Word
nonReservedWord = do
  notFollowedBy reservedWord
  res <- word
  guard $ (not . null) res
  return res

command :: Parser ShellCommand
command = Command <$> liftA2 (:) nonReservedWord
  (many $ inlineSpace *> nonReservedWord)

bashScript :: Parser CommandList
bashScript = fmap CommandList (many (do
  expr <- lexeme (try assign
    <|> try command)
  void (symbol ";" <|> symbol "")
  return expr
  ))
  <* eof
