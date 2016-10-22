{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Phire.SyntaxSpec
  ( spec
  ) where

import Language.Phire.Syntax
import Test.Hspec (describe, it, shouldSatisfy, Spec)

import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "substitute" $ do
    it "performs no substitutions" $ do
      substitute (Map.singleton "x" (Var "y")) (Var "z")
      `shouldSatisfy` \case
        Var "z" -> True
        _ -> False
    it "performs substitutions" $ do
      substitute (Map.singleton "x" (Var "y")) (Var "x")
      `shouldSatisfy` \case
        Var "y" -> True
        _ -> False
