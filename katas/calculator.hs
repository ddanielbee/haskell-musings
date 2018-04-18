module Calculator where

import Data.List.Split (splitWhen)
import Test.Hspec

add :: String -> Int
add =
  foldr
    (\a b ->
       if a == ""
         then b
         else read a + b)
    0 .
  splitWhen (isCommaOrNewLine)

isCommaOrNewLine :: Char -> Bool
isCommaOrNewLine ',' = True
isCommaOrNewLine '\n' = True
isCommaOrNewLine _ = False

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
