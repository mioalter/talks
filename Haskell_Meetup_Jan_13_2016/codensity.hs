{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE Rank2Types #-}

{- Trees and Codens -}

-- A tree monad
data Tree a = Leaf a | Node (Tree a) (Tree a)
	deriving(Eq, Show)

subst :: Tree a -> (a -> Tree b) -> Tree b
subst (Leaf x) k = k x
subst (Node l r) k = Node (subst l k) (subst r k)

instance Monad Tree where
	return = Leaf
	(>>=) = subst

sprout :: Int -> Int -> Tree Int
sprout n = \i -> Node (Leaf (n - 1 - i)) (Leaf (i+1))

fullTree :: Int -> Tree Int
fullTree 1 = Leaf 1
fullTree n = fullTree (n-1) >>= sprout n

zigzag :: Tree Int -> Int
zigzag = zig
	where
		zig (Leaf n) = n
		zig (Node l r) = zag l
		zag (Leaf n) = n
		zag (Node l r) = zig r

-- Codensity monad in general
newtype CodT m a = CodT {runCodT :: forall z . (a -> m z) -> m z}

instance Monad (CodT m) where
	return x = CodT $ \c -> c x
	f >>= g = CodT $ \c -> runCodT(f) (\x -> runCodT(g x) c)

-- Codensity monad for Tree
type Coden a = CodT Tree a

rep :: Tree a -> Coden a
rep t = CodT (t >>=)

abs :: Coden a -> Tree a
abs (CodT p) = p return 
-- the type constructor Leaf
-- is a function of type a -> Tree a

leaf :: a -> Coden a
leaf = return

node :: Coden a -> Coden a -> Coden a
node (CodT p) (CodT q) = CodT (\h -> Node (p h) (q h))

sproden :: Int -> Int -> Coden Int
sproden n = \i -> node (leaf (n - 1 - i)) (leaf (i + 1))

fullCoden :: Int -> Coden Int
fullCoden 1 = leaf 1
fullCoden n = fullCoden (n-1) >>= sproden n