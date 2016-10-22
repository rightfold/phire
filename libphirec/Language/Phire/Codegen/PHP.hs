{-# LANGUAGE OverloadedStrings #-}
module Language.Phire.Codegen.PHP
  ( codegen
  ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import Language.Phire.Syntax (Term(..))

import qualified Data.Text as Text

codegen :: Term -> Text
codegen (Var name) = "$" <> name
codegen (Type universe) = Text.pack (show universe)
codegen _ = "(TODO)"
