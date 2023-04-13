module Main where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Language.PLC.Grammar (TermF(..), TermId(..), TypeF(..), TypeId(..), TypeVar(..))
import Language.PLC.Typing (annotate, freshType, infer, normalize, runTypingM, unify, unify')
import Partial.Unsafe (unsafeCrashWith)

main :: Effect Unit
main = do
  do
    -- let
    --   x = TermId "x"
    --   t = LamTerm x unit (VarTerm x unit)
    -- log $ "t  = " <> show t
    -- ((t' /\ y) /\ sigma) <-
    --   runTypingM
    --     ( do
    --         t' <- annotate t
    --         y <- infer t'
    --         pure $ t' /\ y
    --     )
    --     >>= case _ of
    --         Left err -> unsafeCrashWith err
    --         Right (a /\ sigma) -> pure (a /\ sigma)
    -- log $ "t' = " <> show t'
    -- log $ "y  = " <> show y
    -- log $ "sigma = " <> show sigma
    pure unit
  do
    -- (y /\ sigma) <-
    --   runTypingM
    --     ( do
    --         let
    --           a = VarType $ TypeIdVar $ TypeId "A"
    --           b = VarType $ TypeIdVar $ TypeId "B"
    --         alpha <- freshType
    --         beta <- freshType
    --         join $ unify (ArrType <$> pure a <*> beta) (ArrType <$> alpha <*> pure b)
    --     )
    --     >>= case _ of
    --         Left err -> unsafeCrashWith err
    --         Right (a /\ sigma) -> pure (a /\ sigma)
    -- log $ "y  = " <> show y
    -- log $ "sigma = " <> show sigma
    pure unit
  do
    runTypingM
      ( do
          let
            a = VarType $ TypeIdVar $ TypeId "A"

            b = VarType $ TypeIdVar $ TypeId "B"
          alpha <- freshType
          join $ unify (ArrType <$> pure a <*> pure b) (ArrType <$> alpha <*> alpha)
      )
      >>= case _ of
          Left err -> log $ "typing error: " <> err
          Right (y /\ sigma) -> do
            log $ "y  = " <> show y
            log $ "sigma = " <> show sigma
