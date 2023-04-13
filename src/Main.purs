module Main where

import Data.Tuple.Nested ((/\))
import Language.PLC.Grammar (TermF(..), TermId(..))
import Language.PLC.Typing (annotate, infer, runTypingM)
import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafeCrashWith)

main :: Effect Unit
main = do
  let
    x = TermId "x"

    t = LamTerm x unit (VarTerm x unit)
  log $ "t  = " <> show t
  ((t' /\ y) /\ sigma) <-
    runTypingM
      ( do
          t' <- annotate t
          y <- infer t'
          pure $ t' /\ y
      )
      >>= case _ of
          Left err -> unsafeCrashWith err
          Right (a /\ sigma) -> pure (a /\ sigma)
  log $ "t' = " <> show t'
  log $ "y  = " <> show y
  log $ "sigma = " <> show sigma
