{-# LANGUAGE LambdaCase #-}

module Block3
    ( Parser (..)
    , ok
    , eof
    , satisfy
    , element
    , stream
    , balancedSeq
    , onlyInteger
    , listOfList
    ) where

import Prelude

import Control.Applicative (Alternative (..), liftA2)
import Control.Monad (replicateM)
import Data.Char (isDigit, isSpace)

newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap f pa = Parser $ \s -> do
    (a, t) <- runParser pa s
    return (f a, t)

instance Applicative (Parser s) where
  pure a = Parser $ \s -> Just (a, s)

  pf <*> pa = Parser $ \s -> do
    (f, t1) <- runParser pf s
    (a, t2) <- runParser pa t1
    return (f a, t2)

instance Monad (Parser s) where
  return = pure

  ma >>= f = Parser $ \s -> do
    (a, t) <- runParser ma s
    runParser (f a) t

instance Alternative (Parser s) where
  empty = Parser $ const Nothing
  pa <|> pb = Parser $ \s -> runParser pa s <|> runParser pb s

--------------------------------------------------------------------------------

ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

eof :: Parser s ()
eof = Parser $ \case
  [] -> Just ((), [])
  _  -> Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser $ \case
  []       -> Nothing
  (x : xs) -> if p x then Just (x, xs) else Nothing

element :: Eq s => s -> Parser s s
element c = satisfy (c ==)

stream :: Eq s => [s] -> Parser s [s]
stream = mapM element

--------------------------------------------------------------------------------

balancedSeq :: Parser Char ()
balancedSeq = balancedSeqHelper *> eof
  where
    balancedSeqHelper :: Parser Char ()
    balancedSeqHelper = ( ( element '(' *> balancedSeqHelper <* element ')' )
                        *> balancedSeqHelper)
                    <|> ok

onlyInteger :: Parser Char Int
onlyInteger = integer <* eof

-- this function is useful for listOfList, that is why it is not in 'where'
integer :: Parser Char Int
integer = read <$> (liftA2 (:) (element '-' <|> satisfy isDigit)
                               (many (satisfy isDigit))
               <|> (element '+' *> some (satisfy isDigit)))

--------------------------------------------------------------------------------

listOfList :: Parser Char [[Int]]
listOfList = many (satisfy isSpace) *>
             ((eof *> Parser (const $ Just ([], "")))
         <|> liftA2 (:) (integer >>= intList)
                        (many (skip *> (integer >>= intList))) <* eof)
  where
    intList :: Int -> Parser Char [Int]
    intList n = replicateM n (skip *> integer)
    skip :: Parser Char Char
    skip = many (satisfy isSpace) *> element ',' <* many (satisfy isSpace)
