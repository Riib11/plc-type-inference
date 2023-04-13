module Language.PLC.Typing where

import Language.PLC.Grammar (HoleId(..), Term, TermF(..), Type, TypeF(..), TypeId, TypeVar(..))
import Prelude
import Prim hiding (Type)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.State (StateT)
import Data.Expr (substitute)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafeCrashWith)

type M
  = StateT (Map.Map HoleId Type) (ExceptT String Effect)

-- annotates term with normalized types
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
          y <- m_y
          pure <<< norm
            $ substitute
                ( \tv -> case tv of
                    TypeIdVar i'
                      | i == i' -> y
                    TypeIdVar _ -> VarType tv
                    HoleIdVar _ -> VarType tv
                )
                y2
        _ -> throwError "infer: badly-typed application"

substituteTypeId :: TypeId -> Type -> Type -> M (M Type)
substituteTypeId i y =
  pure <<< norm
    <<< substitute \tv -> case tv of
        TypeIdVar i'
          | i == i' -> y
        _ -> VarType tv

-- expected, inferred
unify :: Type -> Type -> M (M Type)
unify = unsafeCrashWith "TODO"

norm :: Type -> M Type
norm = unsafeCrashWith "TODO"

-- first layer is making unique hole id, second layer is normalization 
freshType :: M (M Type)
freshType = do
  y <- VarType <<< HoleIdVar <<< HoleId <$> liftEffect UUID.genUUID
  pure (norm y)
