module Main (main) where

import Test.Hspec

import Globber

main :: IO ()
main = hspec $ describe "Testing Globber" $ do

    describe "empty pattern" $ do
      it "matches empty string" $
        matchGlob "" "" `shouldBe` True
      it "shouldn't match non-empty string" $
        matchGlob "" "string" `shouldBe` False

    describe "literal match" $ do
      it "matches single character" $
        matchGlob "a" "a" `shouldBe` True
      it "should not match wrong character" $
        matchGlob "a" "b" `shouldBe` False
      it "should not match empty string" $
        matchGlob "a" "" `shouldBe` False
      it "should not match empty pattern" $
        matchGlob "" "a" `shouldBe` False
      it "should match string" $
        matchGlob "abcd" "abcd" `shouldBe` True
      it "should not match wrong string" $
        matchGlob "abcd" "ebcd" `shouldBe` False
      it "should not match wrong string" $
        matchGlob "abcd" "abce" `shouldBe` False
      it "should not match string of wrong length" $
        matchGlob "abcd" "abcde" `shouldBe` False
      it "should not match string of wrong length" $
        matchGlob "abcde" "abcd" `shouldBe` False
      it "should not match string of wrong length" $
        matchGlob "bcd" "abcd" `shouldBe` False
      it "should not match string of wrong length" $
        matchGlob "abcd" "bcd" `shouldBe` False
    
    describe "? match" $ do
      it "should match single character" $
        matchGlob "?" "a" `shouldBe` True
      it "should match string" $
        matchGlob "a?c" "abc" `shouldBe` True
      it "should not match wrong length" $
        matchGlob "ab?c" "abc" `shouldBe` False
