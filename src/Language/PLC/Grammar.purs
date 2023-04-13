module Language.PLC.Grammar where

import Prelude
import Prim hiding (Type)
import Data.Expr (class Expr, joinExpr)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Traversable (class Traversable)
import Data.UUID (UUID)
import Prim as Prim

-- | Kind.
-- | ```
-- | <<kind>> ::= (* ->)^n *
-- | ```
data Kind
  = Kind Int

-- | Type.
type Type
  = TypeF

data TypeVar
  = TypeIdVar TypeId
  | HoleIdVar HoleId

data TypeF x
  = VarType x
  | AppType (TypeF x) (TypeF x)
  | LamType TypeId (TypeF x)
  | ArrType (TypeF x) (TypeF x)

derive instance genericTypeF :: Generic (TypeF x) _

derive instance functorTypeF :: Functor TypeF

derive instance foldableTypeF :: Foldable TypeF

derive instance traversableTypeF :: Traversable TypeF

instance exprTypeF :: Expr TypeF where
  mapExpr = map
  joinExpr (VarType x) = x
  joinExpr (AppType y1 y2) = AppType (joinExpr y1) (joinExpr y2)
  joinExpr (ArrType y1 y2) = ArrType (joinExpr y1) (joinExpr y2)
  joinExpr (LamType x y2) = LamType x (joinExpr y2)

-- | Term.
type Term y
  = TermF y

data TermF y
  = VarTerm TermId y
  | LamTerm TermId (TermF y)
  | AppTerm (TermF y) (TermF y)

derive instance functorTermF :: Functor TermF

derive instance foldableTermF :: Foldable TermF

derive instance traversableTermF :: Traversable TermF

-- | Type name.
newtype TypeId
  = TypeId String

derive instance eqTypeId :: Eq TypeId

derive instance ordTypeId :: Ord TypeId

-- | Term name.
newtype TermId
  = TermId String

derive instance eqTermId :: Eq TermId

derive instance ordTermId :: Ord TermId

-- | Type hole id.
newtype HoleId
  = HoleId UUID

derive instance eqHoleId :: Eq HoleId

derive instance ordHoleId :: Ord HoleId
