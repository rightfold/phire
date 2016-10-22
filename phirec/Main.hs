module Main where

import Language.Phire.Check (runCheck, typeOf)
import Language.Phire.Parse (parse)

import qualified Data.Map as Map
import qualified Data.Text.IO as Text.IO

main :: IO ()
main = do
  text <- Text.IO.getContents
  case parse "<stdin>" text of
    Left err -> print err
    Right term -> do
      print term
      putStr " : "
      putStrLn $ case runCheck (typeOf term) Map.empty of
        Right type_ -> show type_
        Left err -> show err
