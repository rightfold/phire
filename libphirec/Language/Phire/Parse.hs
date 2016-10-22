{-# LANGUAGE OverloadedStrings #-}
module Language.Phire.Parse
  ( parse
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard, void)
import Data.Text (Text)
import Language.Phire.Syntax (Term(..))
import Text.Parsec.Text (Parser)

import qualified Data.Text as Text
import qualified Text.Parsec as P

parse :: String -> Text -> Either P.ParseError Term
parse = P.parse (term <* P.eof)

term :: Parser Term
term = level0
  where
    level0 =     P.try (Var <$> identifier)
             <|> (Type <$> (kType *> pLParen *> integer <* pRParen))

identifier :: Parser Text
identifier = flip P.label "identifier" . lexeme $ do
  name <- fmap Text.pack $ (:) <$> ihead <*> P.many itail
  guard (name `notElem` ["type"])
  pure name
  where
    ihead = P.oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"
    itail = ihead <|> P.oneOf ['0'..'9']

integer :: Parser Int
integer = flip P.label "integer" . lexeme $
  read <$> P.many1 (P.oneOf ['0'..'9'])

kType :: Parser ()
kType = void . lexeme $ P.string "type"

pLParen :: Parser ()
pLParen = void . lexeme $ P.string "("

pRParen :: Parser ()
pRParen = void . lexeme $ P.string ")"

lexeme :: Parser a -> Parser a
lexeme p = P.many space *> p <* P.many space
  where
    space = P.oneOf " \t\r\n"
