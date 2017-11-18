{-
Welcome to your custom Prelude
Export here everything that should always be in your library scope
For more info on what is exported by Protolude check:
https://github.com/sdiehl/protolude/blob/master/Symbols.md
-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE FlexibleInstances    #-}


module Lib.Prelude
    ( module Exports
      , Length, Vec, vec, unVec, Copy, NotZ, nil, (@@), unconsV, eqLen, zipV, tlength
      -- ,ElemAt, AppType(..)
      -- ,(!), Vec(..), toVec
    ) where

import Protolude as Exports
import GHC.TypeLits
import Unsafe.Coerce(unsafeCoerce)

type family Length (l :: [k]) where
    Length '[] = 0
    Length (_ ': xs) = 1 + (Length xs)


type family Copy t (n :: Nat) where
    Copy t 0 = Maybe t
    Copy t n = t -> Copy t (n-1)


type family NotZ (n :: Nat) where
    NotZ 0 = 'False
    NotZ n = 'True





newtype Vec (n :: Nat) a = Vec (Int, [a]) deriving (Eq, Show, Functor, Foldable, Traversable)


tlength :: Foldable t => t a -> Maybe SomeNat
tlength = someNatVal . toInteger . length

vec :: KnownNat n => Proxy n -> [a] -> Maybe (Vec n a)
vec p v | (fromIntegral $ natVal p) == length v = Just $ Vec (length v, v)
        | otherwise = Nothing

unVec :: Vec n a -> [a]
unVec (Vec (_,xs)) = xs

nil :: Vec 0 a
nil = Vec (0, [])

unconsV :: (NotZ n ~ 'True) => Vec n a -> (a , Vec (n-1) a)
unconsV (Vec (n, x:xs)) = (x, Vec (n-1, xs))
unconsV _ = undefined

infixr @@

(@@) :: a -> Vec n a -> Vec (n+1) a
x @@ (Vec (n, xs)) = Vec (n+1, x:xs)


zipV :: Vec n a -> Vec n b -> Vec n (a,b)
zipV (Vec (n, as)) (Vec (_, bs)) = Vec $ (n , zip as bs)

eqLen :: Vec n a -> Vec m b -> Maybe (n :~: m)
eqLen (Vec (n, _)) (Vec (m, _)) | n == m = Just (unsafeCoerce Refl)
                     | otherwise = Nothing



-- class AppType b n where
--     appType :: (NotZ n ~ b) => Copy t n -> Vec n t -> Maybe t



-- instance AppType 'False 0 where
--     appType f v = f

-- instance AppType (NotZ (n - 1)) (n - 1) => AppType 'True n where
--     appType f v = let (x , vs) = unconsV v in appType ((unsafeCoerce f) x) vs


-- (!) :: [a] -> Int -> Maybe a
-- (x:_ ) ! 0         = Just x
-- (x:xs) ! n | n > 0 = xs ! (n-1)
-- _      ! _         = Nothing


-- infixr :>

-- data Vec :: Nat -> * -> * where
--   Nil  :: Vec 0 a
--   (:>) :: a -> Vec n a -> Vec (n+1) a

-- deriving instance Show a => Show (Vec n a)
-- deriving instance Functor (Vec n)
-- deriving instance Foldable (Vec n)
-- deriving instance Traversable (Vec n)


-- instance Eq a => Eq (Vec n a) where
--     Nil == Nil = True
--     (x :> xs) == (y :> ys) = x == y && xs == (unsafeCoerce ys)
--     _ == _ = False

-- toVec :: KnownNat n => Proxy n -> [a] -> Maybe (Vec n a)
-- toVec p xs | fromIntegral (natVal p) == length xs = 
--     Just $ foldr (\x y -> unsafeCoerce $ x :> y) (unsafeCoerce Nil) xs
--            | otherwise = Nothing

