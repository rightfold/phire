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
    it "infers the types of nilary applications" $ do
      runCheck (typeOf (App (Var "x") [])) (Map.singleton "x" (Pi [] (Type 1)))
      `shouldSatisfy` \case
        Right (Type 1) -> True
        _ -> False
    it "infers the types of unary applications" $ do
      runCheck (typeOf (App (Var "x") [Type 2])) (Map.singleton "x" (Pi [("a", Type 3)] (Var "a")))
      `shouldSatisfy` \case
        Right (Type 2) -> True
        _ -> False
    it "infers the types of nilary abstractions" $ do
      runCheck (typeOf (Abs [] (Var "x"))) (Map.singleton "x" (Type 1))
      `shouldSatisfy` \case
        Right (Pi [] (Type 1)) -> True
        _ -> False
    it "infers the types of unary abstractions" $ do
      runCheck (typeOf (Abs [("x", Type 1)] (Var "x"))) Map.empty
      `shouldSatisfy` \case
        Right (Pi [("x", Type 1)] (Type 1)) -> True
        _ -> False
    it "infers the types of non-dependent binary abstractions" $ do
      runCheck (typeOf (Abs [("x", Type 1), ("y", Type 2)] (Var "x"))) Map.empty
      `shouldSatisfy` \case
        Right (Pi [("x", Type 1), ("y", Type 2)] (Type 1)) -> True
        _ -> False
    it "infers the types of dependent binary abstractions" $ do
      runCheck (typeOf (Abs [("x", Type 1), ("y", Var "x")] (Var "y"))) Map.empty
      `shouldSatisfy` \case
        Right (Pi [("x", Type 1), ("y", Var "x")] (Var "x")) -> True
        _ -> False
    it "infers the types of nilary pi terms" $ do
      runCheck (typeOf (Pi [] (Type 1))) Map.empty
      `shouldSatisfy` \case
        Right (Type 2) -> True
        _ -> False
    it "infers the types of unary pi terms" $ do
      runCheck (typeOf (Pi [("x", Type 2)] (Type 1))) Map.empty
      `shouldSatisfy` \case
        Right (Type 3) -> True
        _ -> False
    it "infers the types of non-dependent binary pi terms" $ do
      runCheck (typeOf (Pi [("x", Type 1), ("y", Type 2)] (Var "x"))) Map.empty
      `shouldSatisfy` \case
        Right (Type 3) -> True
        _ -> False
    it "infers the types of dependent binary pi terms" $ do
      runCheck (typeOf (Pi [("x", Type 1), ("y", Var "x")] (Var "y"))) Map.empty
      `shouldSatisfy` \case
        Right (Type 2) -> True
        _ -> False
    it "infers the types of type terms" $ do
      runCheck (typeOf (Type 1)) Map.empty
      `shouldSatisfy` \case
        Right (Type 2) -> True
        _ -> False
    it "infers the types of let terms" $ do
      runCheck (typeOf (Let "x" (Type 1) (Var "x"))) Map.empty
      `shouldSatisfy` \case
        Right (Type 2) -> True
        _ -> False
    it "reports unknown variables" $ do
      runCheck (typeOf (Var "x")) Map.empty
      `shouldSatisfy` \case
        Left (VariableIsNotInScope "x") -> True
        _ -> False
    it "reports type errors in abstraction parameter types" $ do
      runCheck (typeOf (Abs [("x", Var "y")] (Var "x"))) Map.empty
      `shouldSatisfy` \case
        Left (VariableIsNotInScope "y") -> True
        _ -> False
