module Task7
    ( false
    , second
    , third
    ) where

import Prelude

import Data.Either (lefts, rights)

false :: Bool
false = let
          myNull = null :: String -> Bool
          myDot = (.) :: (String -> Bool) -> ([String] -> String) -> [String] -> Bool
          myHead = head :: [String] -> String
          myDollar = ($) :: ([String] -> Bool) -> [String] -> Bool
          myMap = map :: ((String -> String, String) -> String) -> [(String -> String, String)] -> [String]
          myUncurry = uncurry :: ((String -> String) -> String -> String) -> (String -> String, String) -> String
          myId = id :: (String -> String) -> String -> String
          myConcat = (++) :: String -> String -> String
          myTupple = (,) :: (String -> String) -> String -> (String -> String, String)
          myListConstructor = (:) :: (String -> String, String) -> [(String -> String, String)] -> [(String -> String, String)]
          myEmptyList = [] :: [(String -> String, String)]
          myDorian = "Dorian " :: String
          myGrey = "Grey " :: String

          dotNull = myDot myNull :: ([String] -> String) -> [String] -> Bool
          dotNullHead = dotNull myHead :: [String] -> Bool
          uncurryId = myUncurry myId :: (String -> String, String) -> String
          mapWithFunction = myMap uncurryId :: [(String -> String, String)] -> [String]
          concatDorian = myConcat myDorian :: String -> String
          tuppleDorian = myTupple concatDorian :: String -> (String -> String, String)
          tupple = tuppleDorian myGrey :: (String -> String, String)
          listAppl = myListConstructor tupple :: [(String -> String, String)] -> [(String -> String, String)]
          resultList = listAppl myEmptyList :: [(String -> String, String)]
          mappedList = mapWithFunction resultList :: [String]
          useDollar = myDollar dotNullHead :: [String] -> Bool
          result = useDollar mappedList :: Bool
        in result

second :: [(Integer, Integer)]
second = let
           my1 = 1 :: Integer
           my2 = 2 :: Integer
           my6 = 6 :: Integer
           myZip = zip :: [Integer] -> [Integer] -> [(Integer, Integer)]
           myLefts = lefts :: [Either Integer Integer] -> [Integer]
           myRights = rights :: [Either Integer Integer] -> [Integer]
           myListConstructor = (:) :: Either Integer Integer -> [Either Integer Integer] -> [Either Integer Integer]
           myEmptyList = [] :: [Either Integer Integer]
           myLeft = Left :: Integer -> Either Integer Integer
           myRight = Right :: Integer -> Either Integer Integer
           myAdd = (+) :: Integer -> Integer -> Integer
           myPow = (^) :: Integer -> Integer -> Integer

           makeLefts x = myLefts x :: [Integer]
           makeRights x = myRights x :: [Integer]
           zipWithLefts x = myZip (makeLefts x) :: [Integer] -> [(Integer, Integer)]
           lambdaSubterm x = zipWithLefts x (makeRights x) :: [(Integer, Integer)]
           lambdaTerm x = lambdaSubterm x :: [(Integer, Integer)]

           add1 = myAdd my1 :: Integer -> Integer
           get3 = add1 my2 :: Integer
           pow2 = myPow my2 :: Integer -> Integer
           get64 = pow2 my6 :: Integer
           resLeft = myLeft get3 :: Either Integer Integer
           resRight = myRight get64 :: Either Integer Integer
           appendLeft = myListConstructor resLeft :: [Either Integer Integer] -> [Either Integer Integer]
           appendRight = myListConstructor resRight :: [Either Integer Integer] -> [Either Integer Integer]
           listWithRight = appendRight myEmptyList :: [Either Integer Integer]
           list = appendLeft listWithRight :: [Either Integer Integer]

           result = lambdaTerm list :: [(Integer, Integer)]

         in result

third :: Integer -> Bool
third =
        let
            my0 = 0 :: Integer
            my2 = 2 :: Integer
            my4 = 4 :: Integer
            myNot = not :: Bool -> Bool
            myOr = (||) :: Bool -> Bool -> Bool
            myMod = mod :: Integer -> Integer -> Integer
            myEq = (==) :: Integer -> Integer -> Bool

            orNotX = \x -> myOr (myNot x) :: Bool -> Bool
            myImpl = \x y -> orNotX x y :: Bool
            mod2 = \x -> myMod x my2 :: Integer
            eqMod2 = \x -> myEq (mod2 x) :: Integer -> Bool
            mod4 = \x -> myMod x my4 :: Integer
            eqMod4 = \x -> myEq (mod4 x) :: Integer -> Bool
            myIsMod2 = \x -> eqMod2 x my0 :: Bool
            myIsMod4 = \x -> eqMod4 x my0 :: Bool

            implMod4 = \x -> myImpl (myIsMod4 x) :: Bool -> Bool
            implMod2 = \x -> implMod4 x (myIsMod2 x) :: Bool

            in implMod2

          -- let impl = \x y -> not x || y in
          -- let isMod2 = \x -> x `mod` 2 == 0 in
          -- let isMod4 = \x -> x `mod` 4 == 0 in
          --   \x -> (isMod4 x) `impl` (isMod2 x)
