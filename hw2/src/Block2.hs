{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Block2
    ( Expr (..)
    , ArithmeticError (..)
    , MonadFish
    , MonadJoin
    , eval
    , moving
    , returnFish
    , (>=>)
    , returnJoin
    , join
    ) where

import Prelude hiding (drop)

import Control.Monad.State (State, evalState, get, put)
import Data.Sequence (Seq, drop, empty, index, (|>))

--------------------------------------------------------------------------------

-- Несколько веков назад, Папа Римский объявил, что все евреи населяющие Италию
-- должны принять католичество или будут изгнаны от туда. Евреи, конечно, же
-- сильно возмутились - никому не хотелось покидать нажитые места и искать новый
-- дом.
-- Тогда Папа предложил сделку: он проведёт религиозный спор с лидером евреев -
-- если выиграют евреи, они могут остаться; если выиграет папа - евреи примут
-- его условия.
-- Евреи избрали своим представителем старого и мудрого раввина, но была
-- проблема : раввин не разговаривал по-итальянски,а папа - на иврите.
-- Поэтому они договорились что это будет "спор без слов"
-- И вот, в выбранный день раввин и Папа садятся перед друг другом

-- Папа поднял три пальца - раввин в ответ поднял один
-- Затем Папа развёл руками, показывая вокруг себя - на что раввин указал
-- пальцем себе под ноги
-- И, наконец, папа достал хлеб и чашу вина - после чего раввин положил на
-- стол яблоко

-- После этого Папа поднялся на ноги и объявил своё поражение, сказав что раввин
-- был слишком умен и евреи могут остаться в Италии
-- Позже кардиналы спрашивают Папу: что Же там произошло? На что он ответил
-- -Сперва я показал три пальца, символизирующих отца, сына и святого духа - в
-- ответ он показал один палец, напоминая мне, что бог у нас лишь один
-- -Затем я показал руками вокруг, говоря, что бог есть везде - он указал себе
-- под ноги, показывая, что он прямо здесь, с нами
-- -И наконец, когда я достал хлеб и вино, говоря этим, что бог прощает все
-- грехи, он достал яблоко, символизирующее первородный грех. Он обошел меня в
-- каждом вопросе и я не мог продолжать !

-- В это же время, евреи собрались вокруг раввина и просят его рассказать, как
-- ему удалось переспорить папу
-- -Я вообще без понятия что произошло!
-- -Сперва он показал мне, что у евреев есть три дня чтобы уйти отсюда - ну я
-- конечно в ответ показал средний палец
-- -Затем он показал вокруг, что очистить всю страну от нас, на что я показал,
-- что мы остаёмся здесь
-- "А что затем произошло"- спросила женщина из толпы
-- -Да хрен его знает... Он достал свой обед, ну я тоже за ним

--------------------------------------------------------------------------------

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
      put (new_pos, new_sum, new_counted)
      v <- movingHelper n xs
      return $ new_elem : v
    else do
      let new_counted = counted |> x
          (new_pos, new_sum) = (pos + 1, cur_sum + x)
          new_elem = new_sum / fromIntegral new_pos
      put (new_pos, new_sum, new_counted)
      v <- movingHelper n xs
      return $ new_elem : v

--------------------------------------------------------------------------------

class MonadFish m where
    returnFish :: a -> m a
    (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

class MonadJoin m where
    returnJoin :: a -> m a
    join       :: m (m a) -> m a

instance Monad m => MonadJoin m where
  returnJoin = return
  join mm = mm >>= id

instance Monad m => MonadFish m where
  returnFish = return
  f >=> g = \x -> f x >>= g
