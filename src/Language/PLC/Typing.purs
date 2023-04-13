module Language.PLC.Typing where

import Language.PLC.Grammar
import Prelude
import Prim hiding (Type)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.State (StateT)
import Data.Bifunctor (lmap)
import Data.Expr (substitute)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafeCrashWith)

type M
  = StateT (Map.Map HoleId Type) (ExceptT String Effect)

annotate :: Term (Maybe Type) -> M (Term Type)
annotate t = sequence =<< annotate' t

annotate' :: Term (Maybe Type) -> M (Term (M Type))
annotate' (VarTerm x Nothing) = VarTerm <$> pure x <*> freshType

annotate' (VarTerm x (Just y)) = pure $ VarTerm x (norm y)

annotate' (LamTerm x t) = LamTerm x <$> annotate' t

annotate' (AppTerm t1 t2) = AppTerm <$> annotate' t1 <*> annotate' t2

infer :: Term (M Type) -> M (M Type)
infer (VarTerm _ y) = norm <$> y

infer (LamTerm _ t) = do
  y1 <- freshType
  y2 <- infer t
  pure $ ArrType <$> y1 <*> y2

infer (AppTerm t12 t) = do
  m_y12 <- infer t12
  m_y <- infer t
  m_y12
    >>= case _ of
        LamType i y2 -> do
          -- y2 <- m_y2
          -- void $ unify y2' y2
          -- 
          -- subst x for y in y2
          -- 
          let
            y2' = lmap (\i' -> if i == i' then ?a else ?a) y2
          pure $ norm y2'
        _ -> throwError "infer: badly-typed application"

-- expected, inferred
unify :: Type -> Type -> M (M Type)
unify = unsafeCrashWith "TODO"

-- first layer is making unique hole id, second layer is normalization 
freshType :: M (M Type)
freshType = do
  y <- HoleType <<< HoleId <$> liftEffect UUID.genUUID
  pure (norm y)

norm :: Type -> M Type
norm = unsafeCrashWith "TODO"
