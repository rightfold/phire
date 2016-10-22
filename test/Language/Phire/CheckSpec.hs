{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Phire.CheckSpec
  ( spec
  ) where

import Language.Phire.Check
import Language.Phire.Syntax (Term(..))
import Test.Hspec (describe, it, shouldSatisfy, Spec)

import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "typeOf" $ do
    it "infers the types of variables" $ do
      runCheck (typeOf (Var "x")) (Map.singleton "x" (Type 1))
      `shouldSatisfy` \case
        Right (Type 1) -> True
        _ -> False
    it "infers the types of types" $ do
      runCheck (typeOf (Type 1)) Map.empty
      `shouldSatisfy` \case
        Right (Type 2) -> True
        _ -> False
    it "reports unknown variables" $ do
      runCheck (typeOf (Var "x")) Map.empty
      `shouldSatisfy` \case
        Left (VariableIsNotInScope "x") -> True
        _ -> False
