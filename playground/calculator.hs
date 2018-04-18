-- Kata found here: http://osherove.com/tdd-kata-1/
module Calculator where

import Data.List.Split (splitOn, splitWhen)
import Test.Hspec

defaultDelimiters :: String
defaultDelimiters = ",\n"

add :: String -> Int
add str =
  (foldr stringSum 0 .
   dropWhile (== "//") .
   splitWhen (isDelimiter $ extractCustomDelimiter str ++ defaultDelimiters))
    str

stringSum :: String -> Int -> Int
stringSum "" b = b
stringSum a b = read a + b

isDelimiter :: String -> Char -> Bool
isDelimiter xs x = elem x xs

extractCustomDelimiter :: String -> String
extractCustomDelimiter str@('/':_) = takeDelimiter (splitOn "\n" str)
  where
    takeDelimiter [] = ""
    takeDelimiter (x:_) =
      if length x >= 3
        then [x !! 2]
        else ""
extractCustomDelimiter _ = ""

main :: IO ()
main =
  hspec $ do
    describe "The Calculator" $ do
      describe "Has an add function that" $ do
        it "should return 0 when passed an empty string \"\"" $ do
          add "" `shouldBe` 0
        it "should return the same number when passed just one number" $ do
          add "1" `shouldBe` 1
        it "should return the sum of the numbers when passed two numbers" $ do
          add "1,2" `shouldBe` 3
        it "should return the sum of any amount of numbers" $ do
          add "1,2,3" `shouldBe` 6
        it "should also work with \\n as delimiter" $ do
          add "1\n2\n4" `shouldBe` 7
        it "should work with mixed delimiters (, and \\n)" $ do
          add "1\n2,5" `shouldBe` 8
        it
          "should accept a special delimiter syntax inside the string like //[delimiter]\\n[sumString]" $ do
          add "//;\n1;5;3" `shouldBe` 9
