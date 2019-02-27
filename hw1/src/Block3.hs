module Block3
       ( Weekday
       , nextDay
       ) where

data Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

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
      _ -> error "Modulo of positive Int 7 couldn't be neither 0, 1 ...6"
  
  fromEnum Monday    = 0
  fromEnum Tuesday   = 1
  fromEnum Wednesday = 2
  fromEnum Thursday  = 3
  fromEnum Friday    = 4
  fromEnum Saturday  = 5
  fromEnum Sunday    = 6

nextDay :: Weekday -> Weekday
nextDay = succ 

