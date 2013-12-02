module Prelude.Apply

import Prelude.Functor

infixl 2 <$>

class Functor f => Apply (f : Type -> Type) where
    (<$>) : f (a -> b) -> f a -> f b

infixl 2 <$
(<$) : Apply f => f a -> f b -> f a
a <$ b = map const a <$> b

infixl 2 $>
($>) : Apply f => f a -> f b -> f b
a $> b = map (const id) a <$> b

liftA2 : Apply f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = (map f a) <$> b

liftA3 : Apply f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = (map f a) <$> b <$> c
