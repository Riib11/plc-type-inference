module Data.Expr where

import Prelude

class
  Functor e <= Expr e where
  joinExpr :: forall a. e (e a) -> e a

substitute :: forall e x y. Expr e => (x -> e y) -> e x -> e y
substitute sigma = joinExpr <<< map sigma
