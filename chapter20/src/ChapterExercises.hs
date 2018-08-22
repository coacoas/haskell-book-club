module ChapterExercises where

------------------------

data Constant a b = Constant b

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b

------------------------

data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two a b) = f b

------------------------

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

------------------------

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f _ (Three' a b1 b2) = undefined

------------------------

filterF :: ( Applicative f , Foldable t , Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f ta = undefined
