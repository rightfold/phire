-- | Phire abstract syntax tree.
module Language.Phire.Syntax
  ( Term(..)
  ) where

import Data.Text (Text)

-- | A term in the Phire language.
data Term
  -- | A variable term, such as @x@.
  = Var Text

  -- | A application term, such as @f(x)@.
  | App Term ([] Term)

  -- | A abstraction term, such as @fun(x: t) => x@.
  | Abs ([] (Text, Term)) Term

  -- | A pi term, such as @pi(x: t) => u(x)@.
  | Pi ([] (Text, Term)) Term

  -- | A type term, such as @type 1@.
  | Type Int
  deriving (Show)
