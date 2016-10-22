{-# LANGUAGE OverloadedStrings #-}
module Main where

import Language.Phire.Check (runCheck, typeOf)
import Language.Phire.Codegen.PHP (codegen)
import Language.Phire.Syntax (pretty, Term(..))
import Language.Phire.Parse (parse)

import qualified Data.Map as Map
import qualified Data.Text.IO as Text.IO

main :: IO ()
main = do
  putStrLn "== Enter a term! ============"
  text <- Text.IO.getContents
  case parse "<stdin>" text of
    Left err -> print err
    Right term -> do
      case runCheck (typeOf term) prim of
        Right type_ -> do
          putStrLn "== Here is its type! ========"
          Text.IO.putStrLn $ pretty type_
          putStrLn "== Here is the PHP code! ===="
          Text.IO.putStrLn (codegen term)
        Left err -> do
          putStrLn "== Here is a type error! ===="
          print err
  where
    prim = Map.fromList [ ("bool", Type 1)
                        , ("not", Pi [("x", (Var "bool"))] (Var "bool"))
                        ]
