module Block2
    ( Expr (..)
    , ArithmeticError (..)
    , eval
    , moving
    ) where

import           Prelude hiding (drop)

import Control.Monad.State (State, put, evalState, get )
import Data.Sequence (Seq, index, drop, empty, (|>))

data Expr
  = Const Int
  | Sum Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr

data ArithmeticError
  = DivByZero
  | PowByNegative
  deriving (Eq)

instance Show ArithmeticError where
  show DivByZero     = "Division by zero"
  show PowByNegative = "Power to negative number"

eval :: Expr -> Either ArithmeticError Int
eval (Const x) = return x
eval (Sum a b) = eval a >>= (\x -> eval b >>= (\y -> return $ x + y))
eval (Sub a b) = eval a >>= (\x -> eval b >>= (\y -> return $ x - y))
eval (Mul a b) = eval a >>= (\x -> eval b >>= (\y -> return $ x * y))
eval (Div a b) = do
  x <- eval a
  y <- eval b
  case y of
    0 -> Left DivByZero
    _ -> return $ x `div` y
eval (Pow a b) = do
  x <- eval a
  y <- eval b
  if y < 0
    then Left PowByNegative
    else return $ x ^ y

--------------------------------------------------------------------------------

type MovingValue a = [a]
type MovingState a = (Int, a, Seq a)    -- pos, cur_sum, counted_elems

moving :: Fractional a => Int -> [a] -> [a]
moving n arr = evalState (movingHelper n arr) (0, 0, empty)

movingHelper :: Fractional a => Int -> [a] -> State (MovingState a) (MovingValue a)
movingHelper _ [] = return []
movingHelper n (x : xs) = do
  (pos, cur_sum, counted) <- get
  if pos >= n
    then do
      let (rem_el, new_counted) = (index counted 0, drop 1 counted |> x )
          (new_pos, new_sum) = (pos + 1, cur_sum + x - rem_el)
          new_elem = new_sum / fromIntegral n
      Combine with F:\ITMO\FunctionalProgramming\fp-homework\hw2\src\Block2.hs:72:7
      v <- movingHelper n xs
      return $ new_elem : v
    else do
      let new_counted = counted |> x
          (new_pos, new_sum) = (pos + 1, cur_sum + x)
          new_elem = new_sum / fromIntegral new_pos
      put (new_pos, new_sum, new_counted)
      v <- movingHelper n xs
      return $ new_elem : v
