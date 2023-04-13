module Data.Fix where

import Prelude
import Data.Newtype (class Newtype)

{-
-- | Monomorphic fixpoint.
newtype Fix (f :: Type -> Type)
  = Fix (f (Fix f))

type Fix' f
  = f (Fix f)

derive instance newtypeFix :: Newtype (Fix f) _

showFix :: forall f. (Fix' f -> String) -> Fix f -> String
showFix showFix' (Fix ff) = "(Fix " <> showFix' ff <> ")"

eqFix :: forall f. (Fix' f -> Fix' f -> Boolean) -> Fix f -> Fix f -> Boolean
eqFix eqFix' (Fix ff1) (Fix ff2) = eqFix' ff1 ff2

mapFix ∷ forall f1. Functor f1 ⇒ (Fix' f1 → Fix' f1) → Fix f1 → Fix f1
mapFix f = over Fix (f <<< map (over Fix f))
-}
newtype Fix (f :: Type -> Type -> Type) (a :: Type)
  = Fix (Step f a)

type Step (f :: Type -> Type -> Type) a
  = (f (Fix f a) a)

derive instance newtypeFix :: Newtype (Fix f a) _

showFix :: forall f a. ((Fix f a -> String) -> Step f a -> String) -> Fix f a -> String
showFix showStep (Fix step) = showStep (showFix showStep) step

eqFix :: forall f a. (Step f a -> Step f a -> Boolean) -> Fix f a -> Fix f a -> Boolean
eqFix eqStep (Fix step1) (Fix step2) = eqStep step1 step2

mapFix :: forall f a b. ((Fix f a -> Fix f b) -> Step f a -> Step f b) -> Fix f a -> Fix f b
mapFix mapStep (Fix step) = Fix (mapStep (mapFix mapStep) step)

foldrFix :: forall f a b. ((Fix f a -> b -> b) -> Step f a -> b -> b) -> b -> Fix f a -> b
foldrFix foldStep b (Fix step) = foldStep (\fix b' -> foldrFix foldStep b' fix) step b

foldlFix :: forall f a b. ((b -> Fix f a -> b) -> b -> Step f a -> b) -> b -> Fix f a -> b
foldlFix foldStep b (Fix step) = foldStep (\b' fix -> foldlFix foldStep b' fix) b step

foldMapFix :: forall f a m. Monoid m => ((Fix f a -> m) -> Step f a -> m) -> Fix f a -> m
foldMapFix foldMapStep (Fix step) = foldMapStep (foldMapFix foldMapStep) step

traverseFix :: forall f a b m. Applicative m => ((Fix f a -> m (Fix f b)) -> Step f a -> m (Step f b)) -> Fix f a -> m (Fix f b)
traverseFix traverseStep (Fix step) = Fix <$> traverseStep (traverseFix traverseStep) step
