{-# LANGUAGE OverloadedStrings #-}
module Language.Phire.Parse
  ( parse
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard, void)
import Data.Text (Text)
import Language.Phire.Syntax (Term(..))
import Prelude hiding (abs, pi)
import Text.Parsec.Text (Parser)

import qualified Data.Text as Text
import qualified Text.Parsec as P

parse :: String -> Text -> Either P.ParseError Term
parse = P.parse (term <* P.eof)

term :: Parser Term
term = level2
  where
    level2 = P.choice $ map P.try [ abs level2
                                  , pi level2
                                  , let_ level2
                                  , level1
                                  ]
    level1 = app level0
    level0 = P.choice $ map P.try [var, type_, pLParen *> term <* pRParen]

    var = Var <$> identifier
    app next = foldl App <$> next <*> P.many (pLParen *> term `P.sepBy` pComma <* pRParen)
    abs next = do
      kLam
      pLParen
      parameters <- (`P.sepBy` pComma) $ (,) <$> identifier <*> (pColon *> term)
      pRParen
      body <- next
      pure $ Abs parameters body
    pi next = do
      kPi
      pLParen
      parameters <- (`P.sepBy` pComma) $ (,) <$> identifier <*> (pColon *> term)
      pRParen
      resultType <- next
      pure $ Pi parameters resultType
    let_ next = do
      kLet
      name <- identifier
      pEq
      value <- term
      pSemicolon
      body <- next
      pure $ Let name value body
    type_ = Type <$> (kType *> pLParen *> integer <* pRParen)

identifier :: Parser Text
identifier = flip P.label "identifier" . lexeme $ do
  name <- fmap Text.pack $ (:) <$> ihead <*> P.many itail
  guard (name `notElem` ["lam", "pi", "let", "type"])
  pure name
  where
    ihead = P.oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"
    itail = ihead <|> P.oneOf ['0'..'9']

integer :: Parser Int
integer = flip P.label "integer" . lexeme $
  read <$> P.many1 (P.oneOf ['0'..'9'])

kLam :: Parser ()
kLam = void . lexeme $ P.string "lam"

kPi :: Parser ()
kPi = void . lexeme $ P.string "pi"

kLet :: Parser ()
kLet = void . lexeme $ P.string "let"

kType :: Parser ()
kType = void . lexeme $ P.string "type"

pColon :: Parser ()
pColon = void . lexeme $ P.string ":"

pSemicolon :: Parser ()
pSemicolon = void . lexeme $ P.string ";"

pEq :: Parser ()
pEq = void . lexeme $ P.string "="

pComma :: Parser ()
pComma = void . lexeme $ P.string ","

pLParen :: Parser ()
pLParen = void . lexeme $ P.string "("

pRParen :: Parser ()
pRParen = void . lexeme $ P.string ")"

lexeme :: Parser a -> Parser a
lexeme p = P.many space *> p <* P.many space
  where
    space = P.oneOf " \t\r\n"
