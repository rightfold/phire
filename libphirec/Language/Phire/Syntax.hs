{-# LANGUAGE OverloadedStrings #-}
-- | Phire abstract syntax tree.
module Language.Phire.Syntax
  ( Term(..)
  , substitute
  , pretty
  ) where

import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Text (Text)

import qualified Data.Map as Map
import qualified Data.Text as Text

-- | A term in the Phire language.
data Term
  -- | A variable term, such as @x@.
  = Var Text

  -- | A application term, such as @f(x)@.
  | App Term ([] Term)

  -- | A abstraction term, such as @lam(x: t) x@.
  | Abs ([] (Text, Term)) Term

  -- | A pi term, such as @pi(x: t) u(x)@.
  | Pi ([] (Text, Term)) Term

  -- | A type term, such as @type(1)@.
  | Type Int

  -- | A let term, such as @let x = y; x@.
  | Let Text Term Term
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
substitute _ (Type universe) =
  Type universe
substitute substs (Let name value body) =
  Let name
      (substitute substs value)
      (substitute (Map.delete name substs) body)

-- | Format a term.
pretty :: Term -> Text
pretty (Var name) = name
pretty (App callee arguments) =
  callee' <> "(" <> Text.intercalate ", " (map pretty arguments) <> ")"
  where
    callee' = case callee of
      (Var _)   -> pretty callee
      (App _ _) -> pretty callee
      (Type _)  -> pretty callee
      _ -> "(" <> pretty callee <> ")"
pretty (Abs parameters body) =
  "lam(" <> Text.intercalate ", " (map parameter' parameters) <> ") " <> pretty body
  where parameter' (name, type_) = name <> ": " <> pretty type_
pretty (Pi parameters resultType) =
  "pi(" <> Text.intercalate ", " (map parameter' parameters) <> ") " <> pretty resultType
  where parameter' (name, type_) = name <> ": " <> pretty type_
pretty (Type universe) = "type(" <> Text.pack (show universe) <> ")"
pretty (Let name value body) = "let " <> name <> " = " <> pretty value <> ";\n" <> pretty body
