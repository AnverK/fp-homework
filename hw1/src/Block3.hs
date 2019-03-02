module Block3
       ( Weekday
       , nextDay
       , afterDays
       , isWeekend
       , daysToParty
       , City
       , buildCastle
       , buildCulture
       , buildHouse
       , moveLord
       , buildWalls
       , Nat
       , addNat
       , mulNat
       , subNat
       , intToNat
       , natToInt
       , checkEq
       , compareNat
       , BinTree
       , isEmpty
       , getSize
       , find
       , insert
       , fromList
       , remove
       ) where

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
      _ -> error "Modulo of positive Int 7 couldn't be neither 0, 1 ... 6"

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
data Culture = Church | Library deriving (Show)
data House
  = House1 Citizen
  | House2 Citizen Citizen
  | House3 Citizen Citizen Citizen
  | House4 Citizen Citizen Citizen Citizen
  deriving (Show)
data Citizen = Citizen deriving (Show)

data City = City
  { castle  :: Maybe Castle
  , culture :: Maybe Culture
  , houses  :: (House, [House])
  } deriving (Show)

buildCastle :: City -> Either String City
buildCastle City { castle = Just (Castle _ _) } = Left "There is already castle"
buildCastle city  = Right city { castle = Just $ Castle Nothing Nothing }

buildCulture :: City -> Culture -> Either String City
buildCulture City { culture = Just x } obj       =
  Left $ "There is already " ++ show x ++ ". You can't build " ++ show obj
buildCulture city obj = Right city { culture = Just obj }

buildHouse :: City -> [Citizen] -> Either String City
buildHouse _ []              = Left "You can't build house for nobody"
buildHouse city [_]          = Right city { houses = (fst $ houses city,
                               House1 Citizen :
                               snd (houses city))}
buildHouse city [_, _]       = Right city { houses = (fst $ houses city,
                               House2 Citizen Citizen
                               : snd (houses city))}
buildHouse city [_, _, _]    = Right city { houses = (fst $ houses city,
                               House3 Citizen Citizen Citizen :
                               snd (houses city))}
buildHouse city [_, _, _, _] = Right city { houses = (fst $ houses city,
                               House4 Citizen Citizen Citizen Citizen :
                               snd (houses city))}
buildHouse _ _               = Left "You can't build house for such huge family"

moveLord :: City -> Either String City
moveLord City { castle = Nothing }                         =
  Left "There is no castle to move in"
moveLord City { castle = Just (Castle (Just Lord) _) }     =
  Left "There is already lord in the city"
moveLord city@City { castle = Just (Castle Nothing wall) } =
  Right city { castle = Just $ Castle (Just Lord) wall }

-- it's unclear from statements what to return if there are already built
-- walls in city but there is no Lord e.g.. So in this case it returns
-- given city without error. The reason: in future there could be functions
-- like moveOutLord and probably he doesn't crush walls...
buildWalls :: City -> Either String City
buildWalls city@City { castle = Just (Castle _ (Just Walls)) } = Right city
buildWalls city
  | not $ checkLord $ castle city       =
      Left "There is no lord in the city"
  | not $ checkCitizens (houses city) 0 =
      Left "There is less than 10 citizens in the city"
  | otherwise                           =
      Right city { castle = Just $ Castle (Just Lord) (Just Walls) }
  where
    checkLord :: Maybe Castle -> Bool
    checkLord Nothing                    = False
    checkLord (Just (Castle Nothing _ )) = False
    checkLord _                          = True

    checkCitizens :: (House, [House]) -> Int -> Bool
    checkCitizens h acc
      | acc >= 10 = True
      | otherwise =
         case h of
           (House1{}, [])     -> (acc + 1) >= 10
           (House2{}, [])     -> (acc + 2) >= 10
           (House3{}, [])     -> (acc + 3) >= 10
           (House4{}, [])     -> (acc + 4) >= 10
           (House1{}, x : xs) -> checkCitizens (x, xs) (acc + 1)
           (House2{}, x : xs) -> checkCitizens (x, xs) (acc + 2)
           (House3{}, x : xs) -> checkCitizens (x, xs) (acc + 3)
           (House4{}, x : xs) -> checkCitizens (x, xs) (acc + 4)

-- fullCity :: City
-- fullCity = City
--   { castle = Just $ Castle (Just Lord) Nothing
--   , culture = Just Library
--   , houses = (House1 Citizen,
--               [ House3 Citizen Citizen Citizen
--               , House3 Citizen Citizen Citizen
--               , House3 Citizen Citizen Citizen
--               ])
--   }

--------------------------------------------------------------------------------

data Nat = Z | S Nat deriving (Show)

addNat :: Nat -> Nat -> Nat
addNat n Z     = n
addNat n (S k) = addNat (S n) k

mulNat :: Nat -> Nat -> Nat
mulNat _ Z     = Z
mulNat Z _     = Z
mulNat n (S k) = addNat n (mulNat n k)

subNat :: Nat -> Nat -> Nat
subNat Z _         = Z
subNat n Z         = n
subNat (S n) (S k) = subNat n k

intToNat :: Integer -> Nat
intToNat n
  | n <= 0    = Z
  | otherwise = S (intToNat (n - 1))

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

--------------------------------------------------------------------------------

data BinTree a = Leafe | Node (a, [a]) (BinTree a) (BinTree a) deriving (Show)

isEmpty :: BinTree a -> Bool
isEmpty Leafe = True
isEmpty _     = False

getSize :: BinTree a -> Int
getSize Leafe                          = 0
getSize (Node (_, rest) left right)    =
  getSize left + getSize right + length rest + 1

find :: Ord a => BinTree a -> a -> Bool
find Leafe _ = False
find (Node (x, _) left right) el
  | el == x   = True
  | el < x    = find left el
  | otherwise = find right el

insert :: Ord a => BinTree a -> a -> BinTree a
insert Leafe el = Node (el, []) Leafe Leafe
insert (Node cur@(x, rest) left right) el
  | el == x   = Node (el, x : rest) left right
  | el < x    = Node cur (insert left el) right
  | otherwise = Node cur left (insert right el)

fromList :: Ord a => [a] -> BinTree a
fromList []       = Leafe
fromList (x : xs) = insert (fromList xs) x

-- if there is no such element, it returns same tree, not error
-- So if you need to be sure that tree was changed, you can just call find
-- before removing the element.
remove :: Ord a => BinTree a -> a -> BinTree a
remove Leafe _ = Leafe
remove tree@(Node cur@(x, rest) left right) el
  | el == x   =
    case rest of
      []     -> removeHelper tree el
      y : ys -> Node (y, ys) left right
  | el < x    = Node cur (remove left el) right
  | otherwise = Node cur left (remove right el)
  where
    removeHelper :: Ord a => BinTree a -> a -> BinTree a
    removeHelper Leafe _                =
      error "This function called only from BinTree constructored by Node"
    removeHelper (Node _ Leafe Leafe) _ = Leafe
    removeHelper (Node _ l Leafe) _     = l
    removeHelper (Node _ Leafe r) _     = r
    removeHelper (Node _ l r) _         =
      let nonEmptyKey@(key, _) = getMinimum r in
      Node nonEmptyKey l (remove r key)

    getMinimum :: BinTree a -> (a, [a])
    getMinimum Leafe            =
      error "This function called only from BinTree constructored by Node"
    getMinimum (Node e Leafe _) = e
    getMinimum (Node _ ltr _)   = getMinimum ltr
