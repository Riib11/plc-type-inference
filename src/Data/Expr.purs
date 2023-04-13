module Data.Expr where

import Prelude

class Expr e where
  mapExpr :: forall a b. (a -> b) -> e a -> e b
  joinExpr :: forall a. e (e a) -> e a

substitute :: forall e x y. Expr e => (x -> e y) -> e x -> e y
substitute sigma = joinExpr <<< mapExpr sigma
