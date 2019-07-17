module RealWorld.DiffList where

-- unDL - deconstructor
newtype DList a = DL {
    unDL :: [a] -> [a]
}

append :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)

append' :: DList a -> DList a -> DList a
append' (DL xs) (DL ys) = DL (xs . ys)

fromList :: [a] -> DList a
fromList xs = DL (xs ++)

toList :: DList a -> [a]
toList (DL xs) = xs []

-- can we make it a monad somehow ???
-- to get faster operations on those lists