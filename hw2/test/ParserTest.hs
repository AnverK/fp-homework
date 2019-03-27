module ParserTest
    ( spec
    ) where

import Prelude

import Control.Monad (foldM)
import Data.List (intercalate)
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Test.QuickCheck (Gen, choose, forAll, listOf, property, (===))

import Block3 (Parser (..), balancedSeq, element, eof, listOfList, ok,
  onlyInteger, satisfy, stream)

okTest :: SpecWith ()
okTest = describe "ok" $ do
  it "accepts non-empty list" $
    runParser ok "1" `shouldBe` Just ((), "1")
  it "accepts empty list" $
    runParser ok "" `shouldBe` Just ((), "")
  it "accepts any string and do not consume it" $ property $
    \s -> runParser ok (s :: String) == Just ((), s)

eofTest :: SpecWith ()
eofTest = describe "eof" $ do
  it "fails on non-empty list" $
    runParser eof [1 :: Int] `shouldBe` Nothing
  it "accepts empty list" $
    runParser eof ([] :: [Int]) `shouldBe` Just ((), [])
  it "fails on non-empty string (property)" $ property $
    \c s -> runParser eof ((c :: Char) : s) `shouldBe` Nothing

satisfyTest :: SpecWith ()
satisfyTest = describe "satisfy" $ do
  it "accepts for True and non-empty list and consumes" $
    runParser (satisfy ((1 :: Int) ==)) [1, 2, 3] `shouldBe` Just (1, [2, 3])
  it "fails for False" $
    runParser (satisfy (== 'a')) "bac" `shouldBe` Nothing
  it "fails on empty list" $
    runParser (satisfy (const True)) ([] :: [Int]) `shouldBe` Nothing
  it "fails for False (property)" $ property $
    \s -> runParser (satisfy (const False)) (s :: String) `shouldBe` Nothing
  it "accepts for non-empty string True (property) and consumes" $ property $
    \c s -> runParser (satisfy (const True)) ((c :: Char) : s) `shouldBe`
            Just (c, s)

elementTest :: SpecWith ()
elementTest = describe "element" $ do
  it "fails on empty list" $
    runParser (element (1 :: Int)) [] `shouldBe` Nothing
  -- may be it is not so honest but I believe that it is natural property
  it "should work like satisfy with checking of equality to the arg" $
    property $
    \c s -> runParser (element (c :: Char)) s `shouldBe`
            runParser (satisfy (c ==)) s

streamTest :: SpecWith ()
streamTest = describe "element" $ do
  it "accepts equal strings and consumes it all" $ property $
    \s -> runParser (stream (s :: String)) s `shouldBe` Just (s, "")
  it "should work like consecutive element" $ property $
    \s t -> runParser (stream (s :: String)) t `shouldBe`
            runParser (mapM element s) t

balancedSeqTest :: SpecWith ()
balancedSeqTest = describe "balancedSeq" $ do
  it "accepts balanced sequence" $
    runParser balancedSeq "(())()()" `shouldBe` Just ((), "")
  it "fails on sequence with positive balance" $
    runParser balancedSeq "((())" `shouldBe` Nothing
  it "fails on sequence with inner negative balance" $
    runParser balancedSeq "())()(" `shouldBe` Nothing

  it "balance property" $ property $
    forAll genParSequence (\s ->
      runParser balancedSeq s === checkZeroBalance (foldM updateBalance 0 s))

  where
    genParSequence :: Gen String
    genParSequence = listOf $ choose ('(', ')')

    updateBalance :: Int -> Char -> Maybe Int
    updateBalance b c
      | b < 0 = Nothing
      | otherwise = case c of
        '(' -> Just (b + 1)
        ')' -> Just (b - 1)
        _   -> Nothing

    checkZeroBalance :: Maybe Int -> Maybe ((), String)
    checkZeroBalance Nothing = Nothing
    checkZeroBalance (Just b)
      | b == 0    = Just ((), "")
      | otherwise = Nothing

onlyIntegerTest :: SpecWith ()
onlyIntegerTest = describe "onlyInteger" $ do
  it "accepts number" $
    runParser onlyInteger "12" `shouldBe` Just (12, "")
  it "accepts number with -" $
    runParser onlyInteger "-12" `shouldBe` Just (-12, "")
  it "accepts number with +" $
    runParser onlyInteger "+12" `shouldBe` Just (12, "")
  it "fails on not numbers" $
    runParser onlyInteger "gigi" `shouldBe` Nothing
  it "fails on string with number and something else after" $
    runParser onlyInteger "12kek" `shouldBe` Nothing
  it "is almost reverse to read (without +) (property)" $ property $
    \n -> runParser onlyInteger (show n) `shouldBe` Just (n, "")

listOfListTest :: SpecWith ()
listOfListTest = describe "listOfList" $ do
  it "creates [] on empty string" $
    runParser listOfList "" `shouldBe` Just ([], "")
  it "creates [[]] on \"0\"" $
    runParser listOfList "0" `shouldBe` Just ([[]], "")
  it "random example without whitespaces" $
    runParser listOfList "2,1,+10,3,5,-7,2" `shouldBe`
    Just ([ [1, 10], [5, -7, 2] ], "")
  it "random example with whitespaces" $
    runParser listOfList "2, 1,+10  , 3,5,-7, 2" `shouldBe`
    Just ([ [1, 10], [5, -7, 2] ], "")
  it "accepts string from correct list of list (property)" $ property $
    \ll -> runParser listOfList (intercalate ", " $ map myListToString ll)
          `shouldBe` Just (ll, "")

  where
    myListToString :: [Int] -> String
    myListToString l = intercalate ", " $ map show (length l : l)

spec :: SpecWith ()
spec = do
  okTest
  eofTest
  satisfyTest
  elementTest
  streamTest
  balancedSeqTest
  onlyIntegerTest
  listOfListTest
