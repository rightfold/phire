-- | Phire type checking.
module Language.Phire.Check
  ( Check
  , Error(..)
  , runCheck
  , typeOf
  ) where

import Data.Map (Map)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT)
import Data.Text (Text)
import Language.Phire.Syntax (Term(..))

import qualified Control.Monad.Reader as Reader
import qualified Data.Map as Map

-- | Type checking context.
type Check = ReaderT (Map Text Term) (Either Error)

-- | Type checking error.
data Error
  = VariableIsNotInScope Text
  deriving (Show)

-- | Run a computation in a context.
runCheck :: Check a -> Map Text Term -> Either Error a
runCheck = Reader.runReaderT

-- | Compute the type of a term in some context.
typeOf :: Term -> Check Term
typeOf (Var name) = do
  env <- Reader.ask
  case Map.lookup name env of
    Just term -> pure term
    Nothing -> throwError $ VariableIsNotInScope name
typeOf _ = error "not yet implemented"
