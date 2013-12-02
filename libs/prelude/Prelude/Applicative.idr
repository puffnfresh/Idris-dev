module Prelude.Applicative

import Builtins
import Prelude.Apply
import Prelude.Functor

---- Applicative functors/Idioms

class Apply f => Applicative (f : Type -> Type) where
    pure  : a -> f a

liftA : Applicative f => (a -> b) -> f a -> f b
liftA f a = pure f <$> a

infixl 3 <|>
class Applicative f => Alternative (f : Type -> Type) where
    empty : f a
    (<|>) : f a -> f a -> f a

guard : Alternative f => Bool -> f ()
guard a = if a then pure () else empty

when : Applicative f => Bool -> f () -> f ()
when a f = if a then f else pure ()
