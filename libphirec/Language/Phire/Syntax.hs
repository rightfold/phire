-- | Phire abstract syntax tree.
module Language.Phire.Syntax
  ( Term(..)
  , substitute
  ) where

import Data.Map (Map)
import Data.Text (Text)

import qualified Data.Map as Map

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
  deriving (Eq, Ord, Show)

-- | Perform substitutions in a term.
substitute :: Map Text Term -> Term -> Term
substitute substs (Var name) = do
  case Map.lookup name substs of
    Just subst -> subst
    Nothing -> Var name
substitute substs (App callee arguments) =
  App (substitute substs callee)
      (map (substitute substs) arguments)
substitute substs (Abs parameters body) =
  let (substs', parameters') = foldl onParameter (substs, []) parameters
  in Abs parameters' (substitute substs' body)
  where
    onParameter (substs', parameters') (name, type_) =
      ( Map.delete name substs'
      , parameters' ++ [(name, substitute substs' type_)]
      )
substitute substs (Pi parameters resultType) =
  let (substs', parameters') = foldl onParameter (substs, []) parameters
  in Pi parameters' (substitute substs' resultType)
  where
    onParameter (substs', parameters') (name, type_) =
      ( Map.delete name substs'
      , parameters' ++ [(name, substitute substs' type_)]
      )
substitute _ (Type universe) = Type universe
