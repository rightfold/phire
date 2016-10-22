-- | Phire type checking.
module Language.Phire.Check
  ( Check
  , Error(..)
  , runCheck
  , typeOf
  , termUniverse
  ) where

import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT)
import Data.Map (Map)
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
typeOf (Abs parameters body) = do
  env <- Reader.ask
  env' <- foldM rememberParameterType env parameters
  bodyType <- Reader.local (const env') $ typeOf body
  pure $ Pi parameters bodyType
  where
    rememberParameterType env (name, type_) = do
      _ <- Reader.local (const env) $ typeOf type_
      pure $ Map.insert name type_ env
typeOf (Pi parameters resultType) = do
  env <- Reader.ask
  (env', maxParameterTypeUniverse) <- foldM processParameter (env, (-1)) parameters
  resultTypeType <- Reader.local (const env') $ typeOf resultType
  resultTypeUniverse <- Reader.local (const env') $ termUniverse resultTypeType
  pure $ Type (max maxParameterTypeUniverse resultTypeUniverse)
  where
    processParameter (env, acc) (name, type_) = do
      typeType <- Reader.local (const env) $ typeOf type_
      typeUniverse <- Reader.local (const env) $ termUniverse typeType
      pure $ (Map.insert name type_ env, max acc typeUniverse)
typeOf (Type universe) = pure $ Type (universe + 1)
typeOf _ = error "not yet implemented"

-- | Compute the universe of terms in this type.
termUniverse :: Term -> Check Int
termUniverse term = do
  -- TODO: fail when given a term in universe 0.
  type_ <- typeOf term
  case type_ of
    Type n -> pure (n - 1)
    _ -> pure 0
