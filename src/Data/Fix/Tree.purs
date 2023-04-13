-- | Example of using `Fix` to define `Tree`.
module Data.Fix.Tree where

{-
import Prelude
import Data.Array (intercalate)
import Data.Fix (Fix(..), Step, foldMapFix, foldlFix, mapFix, showFix)
import Data.Foldable (class Foldable, foldMap)
import Data.List (List)
import Data.Newtype (class Newtype, over, under, unwrap)
import Data.Traversable (class Traversable)
import Partial.Unsafe (unsafeCrashWith)

-- | `TreeF` is the type-function that we will take the fixpoint of.
data TreeF (t :: Type) (a :: Type)
  = TreeF a (Array t)

derive instance functorTreeF :: Functor (TreeF t)

derive instance foldableTreeF :: Foldable (TreeF t)

derive instance traversableTreeF :: Traversable (TreeF t)

newtype TreeR a t
  = TreeR (TreeF t a)

-- | `Tree` is a type wrapper `Fix (TreeF a)` for implmenting typeclass
-- | instances.
newtype Tree (a :: Type)
  = Tree (Fix TreeF a)

derive instance newtypeTree :: Newtype (Tree a) _

instance functorTree :: Functor Tree where
  map f = over Tree $ mapFix \mu (TreeF a ts) -> TreeF (f a) (mu <$> ts)

instance foldableTree :: Foldable Tree where
  foldl f b = unwrap >>> foldlFix (\mu b' (TreeF a ts) -> ?a) b
  foldr = ?a
  foldMap f = unwrap >>> foldMapFix \mu -> foldMap ?a

-- f a <> foldMap mu ts
instance showTree :: Show a => Show (Tree a) where
  show = unwrap >>> showFix \mu (TreeF a ts) -> "(Tree " <> show a <> " " <> prettyArray (mu <$> ts) <> ")"
    where
    prettyArray xs = "[" <> intercalate ", " xs <> "]"

tree :: forall a. a -> (Array (Tree a)) -> Tree a
tree a ts = Tree $ Fix $ TreeF a (unwrap <$> ts)

instance showTree :: Show a => Show (Tree a) where
  show (Tree ff) = "(Tree " <> showF ff <> ")"
    where
    -- alpha-expanded in order to bypass purescript's termination checker
    showF ff' = showFix (showTreeF showF) ff'

    showTreeF showT (TreeF a ts) = "(TreeF " <> show a <> " " <> prettyArray (showT <$> ts) <> ")"

    prettyArray xs = "[" <> intercalate ", " xs <> "]"

instance functorTree :: Functor Tree where
  map f (Tree ff) = Tree $ mapF ff
    where
    mapF ff' = mapFix ?a ff'

t0 :: Tree Int
t0 = tree 0 [ tree 1 [] ]

t1 :: Tree Int
t1 =
  tree 0
    [ tree 1
        [ tree 2
            [ tree 3 []
            , tree 4 []
            ]
        , tree 5
            [ tree 6 []
            , tree 7 []
            ]
        ]
    , tree 8
        [ tree 9
            [ tree 10 []
            , tree 11 []
            ]
        , tree 12
            [ tree 13 []
            , tree 14 []
            ]
        ]
    ]
-}
