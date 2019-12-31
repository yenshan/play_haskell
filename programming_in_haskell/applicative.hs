
class Functor f => Applicative f where
	pure :: a -> f a
	(<*>) :: f (a -> b) -> f a -> f b

data Maybe' a = Nothing' | Just' a deriving Show

instance Functor Maybe' where
	-- fmap :: (a -> b) -> f a -> f b
	fmap _ Nothing' = Nothing'
	fmap g (Just' a) = Just' (g a)

instance Applicative Maybe' where
	-- pure :: a -> f a
	pure = Just'

	Nothing' <*> _ = Nothing'
	(Just' g) <*> mx = fmap g mx

addA ::(Num a, Applicative f) => f a -> f a -> f a
addA a b = pure (+) <*> a <*> b

sumA :: (Num a, Applicative f) => [f a] -> f a
sumA [x] = x
sumA (x:xs) = pure (+) <*> x <*> sumA xs

