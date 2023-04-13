module Language.PLC.Typing where

import Prelude
import Prim hiding (Type)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.State (StateT, get, modify)
import Data.Array (elem)
import Data.Expr (substitute)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (foldMap, sequence)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Class (liftEffect)
import Language.PLC.Grammar (HoleId(..), Term, TermF(..), Type, TypeF(..), TypeId, TypeVar(..))

type TypingM
  = StateT (Map.Map HoleId Type) (ExceptT String Effect)

-- | Annotate term with normalized types.
annotate :: Term (Maybe Type) -> TypingM (Term Type)
annotate t = sequence =<< annotate' t

-- | Annotate term with not-yet-normalize types.
annotate' :: Term (Maybe Type) -> TypingM (Term (TypingM Type))
annotate' (VarTerm x Nothing) = VarTerm <$> pure x <*> freshType

annotate' (VarTerm x (Just y)) = pure $ VarTerm x (normalize y)

annotate' (LamTerm x t) = LamTerm x <$> annotate' t

annotate' (AppTerm t1 t2) = AppTerm <$> annotate' t1 <*> annotate' t2

-- | Infer not-yet-normalize type of term.
infer :: Term (TypingM Type) -> TypingM (TypingM Type)
infer (VarTerm _ y) = normalize <$> y

infer (LamTerm _ t) = do
  y1 <- freshType
  y2 <- infer t
  pure $ ArrType <$> y1 <*> y2

infer (AppTerm t12 t1) = do
  -- infer type of applicant
  m_y12 <- infer t12
  -- applicant must be a function type, so unify it's type with fresh funciton
  -- type
  m_y1 <- freshType
  m_y2 <- freshType
  void $ unify (ArrType <$> m_y1 <*> m_y2) m_y12
  -- infer type of t1
  m_y1' <- infer t1
  -- the type of t1 must unify with the domain of t12
  void $ unify m_y1 m_y1'
  -- output type 
  pure m_y2

-- expected, inferred
unify :: TypingM Type -> TypingM Type -> TypingM (TypingM Type)
unify m_y1 m_y2 = join $ unify' <$> m_y1 <*> m_y2

-- expected, inferred
unify' :: Type -> Type -> TypingM (TypingM Type)
unify' (VarType (HoleIdVar h)) y = do
  assertNonoccurs h y
  void $ modify $ Map.insert h y
  pure $ normalize y

unify' y (VarType (HoleIdVar h)) = do
  assertNonoccurs h y
  void $ modify $ Map.insert h y
  pure $ normalize y

unify' (VarType (TypeIdVar i1)) (VarType (TypeIdVar i2))
  | i1 == i2 = pure $ normalize $ VarType (TypeIdVar i1)

-- assume types are normal, so must be neutral
unify' (AppType y11 y12) (AppType y21 y22) = do
  y1 <- unify' y11 y21
  y2 <- unify' y12 y22
  map normalize $ AppType <$> y1 <*> y2

unify' (ArrType y11 y12) (ArrType y21 y22) = do
  y1 <- unify' y11 y21
  y2 <- unify' y12 y22
  map normalize $ ArrType <$> y1 <*> y2

unify' y1 y2 = throwError $ "[unify] cannot unify `" <> show y1 <> "` with `" <> show y2 <> "`"

-- | Substitute a few type id for a concrete type in a type.
substituteTypeId :: TypeId -> TypingM Type -> TypingM Type -> TypingM Type
substituteTypeId i m_y' m_y = do
  y <- m_y
  y' <- m_y'
  normalize
    $ substitute
        ( \tv -> case tv of
            TypeIdVar i'
              | i == i' -> y'
            _ -> VarType tv
        )
        y

-- | Rename i1 to i2 in y.
renameTypeId :: TypeId -> TypeId -> Type -> Type
renameTypeId i1 i2 =
  map \tv -> case tv of
    TypeIdVar i1'
      | i1 == i1' -> TypeIdVar i2
    _ -> tv

-- | Normalize a type, and 
normalize :: Type -> TypingM Type
normalize y = do
  sigma <- get
  pure
    $ substitute
        ( \tv -> case tv of
            HoleIdVar h
              | Just y' <- Map.lookup h sigma -> y'
            _ -> VarType tv
        )
        y

assertNonoccurs :: HoleId -> Type -> TypingM Unit
assertNonoccurs h y = do
  let
    hs =
      foldMap
        ( case _ of
            HoleIdVar h' -> [ h' ]
            _ -> []
        )
        y
  when (h `elem` hs) $ throwError $ "[assertNonoccurs] failed nonoccurence check of `" <> show h <> "` in `" <> show y <> "`"

-- | First layer is making unique hole id, second layer is normalization 
freshType :: TypingM (TypingM Type)
freshType = do
  y <- VarType <<< HoleIdVar <<< HoleId <$> liftEffect UUID.genUUID
  pure (normalize y)
