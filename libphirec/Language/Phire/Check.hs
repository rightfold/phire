-- | Phire type checking.
module Language.Phire.Check
  ( Check
  , Error(..)
  , runCheck
  , typeOf
  , subsumes
  , termUniverse
  ) where

import Control.Monad (foldM, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT)
import Data.Map (Map)
import Data.Text (Text)
import Language.Phire.Syntax (substitute, Term(..))

import qualified Control.Monad.Reader as Reader
import qualified Data.Map as Map

-- | Type checking context.
type Check = ReaderT (Map Text Term) (Either Error)

-- | Type checking error.
data Error
  = VariableIsNotInScope Text
  | TermIsNotCallable Term Term
  | WrongAmountOfArguments
  | IncompatibleTypes Term Term
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
typeOf (App callee arguments) = do
  calleeType <- typeOf callee
  case calleeType of
    Pi parameters resultType -> do
      when (length arguments /= length parameters) $
        throwError $ WrongAmountOfArguments
      substitutions <- foldM processArgument Map.empty (arguments `zip` parameters)
      pure $ substitute substitutions resultType
    _ -> throwError $ TermIsNotCallable callee calleeType
  where
    processArgument substitutions (argument, (parameterName, parameterType)) = do
      argumentType <- typeOf argument
      let parameterType' = substitute substitutions parameterType
      when (not (parameterType' `subsumes` argumentType)) $
        throwError $ IncompatibleTypes argumentType parameterType'
      pure $ Map.insert parameterName argument substitutions
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

-- | Compute if one type subsumes another.
subsumes :: Term -> Term -> Bool
subsumes =
  -- TODO: alpha-rename.
  (==)

-- | Compute the universe of terms in this type.
termUniverse :: Term -> Check Int
termUniverse term = do
  -- TODO: fail when given a term in universe 0.
  type_ <- typeOf term
  case type_ of
    Type universe -> pure (universe - 1)
    _ -> pure 0
