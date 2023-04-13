-- | Grammar of the (implicitly) polymorphic lambda calculus, for use with
-- | Hindley-Milter type inference.
module Language.PLC.Grammar where

import Prelude
import Prim hiding (Type)
import Data.Expr (class Expr, joinExpr)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)
import Data.UUID (UUID)
import Effect.Ref (write)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Prim as Prim

-- | Type.
type Type
  = TypeF TypeVar

data TypeVar
  = TypeIdVar TypeId
  | HoleIdVar HoleId

derive instance genericTypeVar :: Generic TypeVar _

instance showTypeVar :: Show TypeVar where
  show x = genericShow x

data TypeF x
  = VarType x
  | AppType (TypeF x) (TypeF x)
  | ArrType (TypeF x) (TypeF x)

derive instance genericTypeF :: Generic (TypeF x) _

instance showTypeF :: Show x => Show (TypeF x) where
  show x = genericShow x

derive instance functorTypeF :: Functor TypeF

derive instance foldableTypeF :: Foldable TypeF

derive instance traversableTypeF :: Traversable TypeF

instance exprTypeF :: Expr TypeF where
  joinExpr (VarType x) = x
  joinExpr (ArrType y1 y2) = ArrType (joinExpr y1) (joinExpr y2)
  joinExpr (AppType y1 y2) = AppType (joinExpr y1) (joinExpr y2)

-- | Term.
type Term y
  = TermF y

data TermF y
  = VarTerm TermId y
  | LamTerm TermId y (TermF y)
  | AppTerm (TermF y) (TermF y)

derive instance genericTermF :: Generic (TermF y) _

instance showTermF :: Show y => Show (TermF y) where
  show x = genericShow x

derive instance functorTermF :: Functor TermF

derive instance foldableTermF :: Foldable TermF

derive instance traversableTermF :: Traversable TermF

-- | Type name.
newtype TypeId
  = TypeId String

derive newtype instance showTypeId :: Show TypeId

derive instance eqTypeId :: Eq TypeId

derive instance ordTypeId :: Ord TypeId

-- | Term name.
newtype TermId
  = TermId String

derive newtype instance showTermId :: Show TermId

derive instance eqTermId :: Eq TermId

derive instance ordTermId :: Ord TermId

-- | Type hole id.
newtype HoleId
  = HoleId UUID

instance showHoleId :: Show HoleId where
  show (HoleId uuid) =
    unsafePerformEffect do
      m <- Ref.read holeUUIDs
      case Map.lookup uuid m of
        Nothing -> do
          -- if we haven't already shown this hole id, then insert into map,
          -- where index is size of map
          let
            i = Map.size m
          write (Map.insert uuid i m) holeUUIDs
          pure $ "?" <> show i
        Just i -> do
          -- we've already showm this hole id, show we can use it's known index 
          pure $ "?" <> show i

holeUUIDs :: Ref.Ref (Map.Map UUID Int)
holeUUIDs = unsafePerformEffect $ Ref.new Map.empty

derive instance eqHoleId :: Eq HoleId

derive instance ordHoleId :: Ord HoleId
