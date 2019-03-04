{-# LANGUAGE InstanceSigs #-}

module Block3
       ( Weekday (..)
       , nextDay
       , afterDays
       , isWeekend
       , daysToParty
       , City (..)
       , buildCastle
       , buildCulture
       , buildHouse
       , moveLord
       , buildWalls
       , Nat (..)
       , addNat
       , mulNat
       , subNat
       , intToNat
       , natToInt
       , checkEq
       , compareNat
       , isEven
       , divNat
       , modNat
       , BinTree (..)
       , Castle (..)
       , Citizen (..)
       , Culture (..)
       , House (..)
       , isEmpty
       , getSize
       , find
       , insert
       , fromList
       , remove
       ) where

import Prelude

import Data.Foldable (foldr)
import Data.List (length)
import Data.List.NonEmpty (NonEmpty (..))
import Text.Show (show)

data Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show)

instance Enum Weekday where
  toEnum n =
    case ((n `mod` 7) + 7) `mod` 7 of
      0 -> Monday
      1 -> Tuesday
      2 -> Wednesday
      3 -> Thursday
      4 -> Friday
      5 -> Saturday
      6 -> Sunday
      _ -> error "Modulo of positive Int of 7 couldn't be neither 0, 1 ... 6"

  fromEnum Monday    = 0
  fromEnum Tuesday   = 1
  fromEnum Wednesday = 2
  fromEnum Thursday  = 3
  fromEnum Friday    = 4
  fromEnum Saturday  = 5
  fromEnum Sunday    = 6

nextDay :: Weekday -> Weekday
nextDay = succ

afterDays :: Weekday -> Int -> Weekday
afterDays day n = toEnum $ fromEnum day + n

isWeekend :: Weekday -> Bool
isWeekend day = fromEnum day >= 5

daysToParty :: Weekday -> Int
daysToParty day = (11 - fromEnum day) `mod` 7

--------------------------------------------------------------------------------

data Castle = Castle (Maybe Lord) (Maybe Walls) deriving (Show)
data Lord = Lord deriving (Show)
data Walls = Walls deriving (Show)
data Culture
  = Church
  | Library
  deriving (Show)
data House
  = House1 Citizen
  | House2 Citizen Citizen
  | House3 Citizen Citizen Citizen
  | House4 Citizen Citizen Citizen Citizen
  deriving (Show)
data Citizen = Citizen deriving (Show)

data City = City
  { cCastle  :: Maybe Castle
  , cCulture :: Maybe Culture
  , cHouses  :: NonEmpty House
  } deriving (Show)

buildCastle :: City -> Either String City
buildCastle City { cCastle = Just (Castle _ _) } =
  Left "There is already cCastle"
buildCastle city                                 =
  Right city { cCastle = Just $ Castle Nothing Nothing }

buildCulture :: City -> Culture -> Either String City
buildCulture City { cCulture = Just x } obj       =
  Left $ "There is already " ++ show x ++ ". You can't build " ++ show obj
buildCulture city obj                             =
  Right city { cCulture = Just obj }

buildHouse :: City -> [Citizen] -> Either String City
buildHouse _ []              = Left "You can't build house for nobody"
buildHouse city citizens     = let (x :| xs) = cHouses city in
  case citizens of
    [_]          -> Right city
      { cHouses = House1 Citizen :| (x : xs) }
    [_, _]       -> Right city
      { cHouses = House2 Citizen Citizen :| (x : xs) }
    [_, _, _]    -> Right city
      { cHouses = House3 Citizen Citizen Citizen :| (x : xs) }
    [_, _, _, _] -> Right city
      { cHouses = House4 Citizen Citizen Citizen Citizen :| (x : xs) }
    _            -> Left "You can't build house for such huge family"

moveLord :: City -> Either String City
moveLord City { cCastle = Nothing }                         =
  Left "There is no cCastle to move in"
moveLord City { cCastle = Just (Castle (Just Lord) _) }     =
  Left "There is already lord in the city"
moveLord city@City { cCastle = Just (Castle Nothing wall) } =
  Right city { cCastle = Just $ Castle (Just Lord) wall }

-- it's unclear from statements what to return if there are already built
-- walls in city but there is no Lord e.g.. So in this case it returns
-- given city without error. The reason: in future there could be functions
-- like moveOutLord and probably he doesn't crush walls...
buildWalls :: City -> Either String City
buildWalls city@City { cCastle = Just (Castle _ (Just Walls)) } = Right city
buildWalls city
  | not $ checkLord $ cCastle city       =
      Left "There is no lord in the city"
  | not $ checkCitizens (cHouses city) 0 =
      Left "There is less than 10 citizens in the city"
  | otherwise                           =
      Right city { cCastle = Just $ Castle (Just Lord) (Just Walls) }
  where
    checkLord :: Maybe Castle -> Bool
    checkLord Nothing                    = False
    checkLord (Just (Castle Nothing _ )) = False
    checkLord _                          = True

    checkCitizens :: NonEmpty House -> Int -> Bool
    checkCitizens h acc
      | acc >= 10 = True
      | otherwise =
         case h of
           (House1{} :| [])       -> (acc + 1) >= 10
           (House2{} :| [])       -> (acc + 2) >= 10
           (House3{} :| [])       -> (acc + 3) >= 10
           (House4{} :| [])       -> (acc + 4) >= 10
           (House1{} :| (x : xs)) -> checkCitizens (x :| xs) (acc + 1)
           (House2{} :| (x : xs)) -> checkCitizens (x :| xs) (acc + 2)
           (House3{} :| (x : xs)) -> checkCitizens (x :| xs) (acc + 3)
           (House4{} :| (x : xs)) -> checkCitizens (x :| xs) (acc + 4)

-- fullCity :: City
-- fullCity = City
--   { cCastle = Just $ Castle (Just Lord) Nothing
--   , cCulture = Just Library
--   , cHouses = (House1 Citizen,
--               [ House3 Citizen Citizen Citizen
--               , House3 Citizen Citizen Citizen
--               , House3 Citizen Citizen Citizen
--               ])
--   }

--------------------------------------------------------------------------------

data Nat
  = Z
  | S Nat
  deriving (Show)

addNat :: Nat -> Nat -> Nat
addNat n Z     = n
addNat n (S k) = addNat (S n) k

mulNat :: Nat -> Nat -> Nat
mulNat _ Z     = Z
mulNat Z _     = Z
mulNat n (S k) = addNat n $ mulNat n k

subNat :: Nat -> Nat -> Nat
subNat Z _         = Z
subNat n Z         = n
subNat (S n) (S k) = subNat n k

intToNat :: Integer -> Nat
intToNat n
  | n <= 0    = Z
  | otherwise = S $ intToNat (n - 1)

natToInt :: Nat -> Integer
natToInt Z     = 0
natToInt (S k) = 1 + natToInt k

checkEq :: Nat -> Nat -> Bool
checkEq Z Z         = True
checkEq _ Z         = False
checkEq Z _         = False
checkEq (S n) (S k) = checkEq n k

compareNat :: Nat -> Nat -> Ordering
compareNat Z     Z     = EQ
compareNat Z _         = LT
compareNat _ Z         = GT
compareNat (S n) (S k) = compareNat n k

isEven :: Nat -> Bool
isEven Z     = True
isEven (S k) = not $ isEven k

divNat :: Nat -> Nat -> Nat
divNat _ Z = error "divide by zero"
divNat n m =
  case compareNat n m of
    LT -> Z
    _  -> S $ divNat (subNat n m) m

modNat :: Nat -> Nat -> Nat
modNat _ Z = error "divide by zero"
modNat n m = subNat n $ mulNat (divNat n m) m

--------------------------------------------------------------------------------

data BinTree a
  = Leaf
  | Node (NonEmpty a) (BinTree a) (BinTree a)
  deriving (Show)

isEmpty :: BinTree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

getSize :: BinTree a -> Int
getSize Leaf                          = 0
getSize (Node (_ :| rest) left right)    =
  getSize left + getSize right + length rest + 1

find :: Ord a => BinTree a -> a -> Bool
find Leaf _ = False
find (Node (x :| _) left right) el
  | el == x   = True
  | el < x    = find left el
  | otherwise = find right el

insert :: Ord a => BinTree a -> a -> BinTree a
insert Leaf el = Node (el :| []) Leaf Leaf
insert (Node cur@(x :| rest) left right) el
  | el == x   = Node (el :| (x : rest)) left right
  | el < x    = Node cur (insert left el) right
  | otherwise = Node cur left (insert right el)

fromList :: Ord a => [a] -> BinTree a
fromList []       = Leaf
fromList (x : xs) = insert (fromList xs) x

-- if there is no such element, it returns same tree, not error
-- So if you need to be sure that tree was changed, you can just call find
-- before removing the element.
remove :: Ord a => BinTree a -> a -> BinTree a
remove Leaf _ = Leaf
remove tree@(Node cur@(x :| rest) left right) el
  | el == x   =
    case rest of
      []     -> removeHelper tree el
      y : ys -> Node (y :| ys) left right
  | el < x    = Node cur (remove left el) right
  | otherwise = Node cur left (remove right el)
  where
    removeHelper :: Ord a => BinTree a -> a -> BinTree a
    removeHelper Leaf _                =
      error "This function called only from BinTree constructored by Node"
    removeHelper (Node _ Leaf Leaf) _ = Leaf
    removeHelper (Node _ l Leaf) _     = l
    removeHelper (Node _ Leaf r) _     = r
    removeHelper (Node _ l r) _         =
      let nonEmptyKey@(key :| _) = getMinimum r
      in Node nonEmptyKey l (remove r key)

    getMinimum :: BinTree a -> NonEmpty a
    getMinimum Leaf            =
      error "This function called only from BinTree constructored by Node"
    getMinimum (Node e Leaf _) = e
    getMinimum (Node _ ltr _)   = getMinimum ltr

instance Foldable BinTree where
  foldr :: (a -> b -> b) -> b -> BinTree a -> b
  foldr _ z Leaf                   = z
  foldr f z (Node vals left right) =
    foldr f (foldr f (foldr f z right) vals ) left
