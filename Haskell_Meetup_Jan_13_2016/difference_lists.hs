{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Data.Monoid

{- Lists and Difference Lists -}

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

type EList a = [a] -> [a]

rep :: [a] -> EList a
rep xs = (xs ++)

absp :: EList a -> [a]
absp f = f []

rev' :: [a] -> EList a 
rev' [] = rep []
rev' (x:xs) = rev' xs . rep [x]

fastReverse :: [a] -> [a]
fastReverse = absp . rev'

instance Monoid (EList a) where
	mappend = (.)
	mempty = id