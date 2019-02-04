{-# LANGUAGE OverloadedStrings #-}

module BBLispSpec
    (
      -- * Spec
      spec
    ) where

import qualified Data.ByteString as Bs
import qualified Data.Map.Strict as Map

import BBLisp (builtinBindings, runTemplate, runTemplateWith)
import BBLisp.SyntaxTree (List(..))
import Test.Hspec

import qualified Templates as Tmp

-- | Spec for `BBLisp`.
spec :: Spec
spec =
    describe "runTemplate" $ do
        it "runs template containing booleans" $
            runTemplate Tmp.tempBool `shouldBe` Right resultBool
        it "runs template containing nil" $
            runTemplate Tmp.tempNil `shouldBe` Right resultNil
        it "runs template containing literals" $
            runTemplate Tmp.tempLit `shouldBe` Right resultLit
        it "runs template containing multiple strings" $
            runTemplate Tmp.tempStrings `shouldBe` Right resultStrings
        it "runs template containing dictionary literal" $
            runTemplate Tmp.tempDict1 `shouldBe` Right resultDict1
        it "runs template containing nested dictionary" $
            runTemplateWithTest planets Tmp.tempDict2
            `shouldBe` Right resultDict2
        it "runs template containing vector literal" $
            runTemplate Tmp.tempVector1 `shouldBe` Right resultVector1
        it "runs template containing nested vector" $
            runTemplate Tmp.tempVector2 `shouldBe` Right resultVector2
        it "runs empty template" $
            runTemplate Tmp.tempEmpty `shouldBe` Right resultEmpty
        it "runs template containing only comment" $
            runTemplate Tmp.tempComment1 `shouldBe` Right resultComment1
        it "runs template containing comment" $
            runTemplate Tmp.tempComment2 `shouldBe` Right resultComment2
        it "runs template containing if" $
            runTemplateWithTest name Tmp.tempIf `shouldBe` Right resultIf
        it "runs template containing unless" $
            runTemplate Tmp.tempUnless `shouldBe` Right resultUnless
        it "runs template containing with" $
            runTemplateWithTest words' Tmp.tempWith `shouldBe` Right resultWith
        it "runs template containing arithmetic" $
            runTemplate Tmp.tempArith `shouldBe` Right resultArith
  where
    runTemplateWithTest = runTemplateWith . Map.union builtinBindings
    planets = Map.singleton "$planets" $ Dict $ Map.singleton "earth" $
        Dict $ Map.singleton "weight" $ String "5.97237E24"
    name = Map.singleton "$name" $ String "Gumball"
    words' = Map.singleton "$list" $
        List [String "fire", String "ground", String "metal"]

-- | Expected result for `tempBool`.
resultBool :: Bs.ByteString
resultBool = "This is true. That is false."

-- | Expected result for `tempNil`.
resultNil :: Bs.ByteString
resultNil = "Nothing is here, except ."

-- | Expected result for `tempLit`.
resultLit :: Bs.ByteString
resultLit = "First 10 digits of pi is 3.1415926535."

-- | Expected result for `tempStrings`.
resultStrings :: Bs.ByteString
resultStrings = "Hello world"

-- | Expected result for `tempDict1`.
resultDict1 :: Bs.ByteString
resultDict1 = "There are 5 apples."

-- | Expected result for `tempDict2`.
resultDict2 :: Bs.ByteString
resultDict2 = "Earth weights 5.97237E24 kg."

-- | Expected result for `tempVector1`.
resultVector1 :: Bs.ByteString
resultVector1 = "Third prime number is 5."

-- | Expected result for `tempVector2`.
resultVector2 :: Bs.ByteString
resultVector2 = "Grid (1, 2) is 4."

-- | Expected result for `tempEmpty`.
resultEmpty :: Bs.ByteString
resultEmpty = ""

-- | Expected result for `tempComment1`.
resultComment1 :: Bs.ByteString
resultComment1 = ""

-- | Expected result for `tempComment2`.
resultComment2 :: Bs.ByteString
resultComment2 = "hello world."

-- | Expected result for `tempIf`.
resultIf :: Bs.ByteString
resultIf = "Hello Gumball."

-- | Expected result for `tempUnless`.
resultUnless :: Bs.ByteString
resultUnless = "Hello world."

-- | Expected result for `tempWith`.
resultWith :: Bs.ByteString
resultWith = "water and fire and ground and metal and "

-- | Expected result for `tempArith`.
resultArith :: Bs.ByteString
resultArith = "The answer is 42."
