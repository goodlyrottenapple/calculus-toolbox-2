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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}


module Lib.Prelude
    ( module Exports, MonadThrowJSON(..)
      , Length, Vec, vec, Copy, NotZ, pattern Nil, pattern (:>), eqLen, zipV, tlength
      -- ,ElemAt, AppType(..)
      -- ,(!), Vec(..), toVec
    ) where

import Protolude as Exports
import GHC.TypeLits
import Unsafe.Coerce(unsafeCoerce)

import Data.Aeson

type family Length (l :: [k]) where
    Length '[] = 0
    Length (_ ': xs) = 1 + (Length xs)


type family Copy t (n :: Nat) where
    Copy t 0 = Maybe t
    Copy t n = t -> Copy t (n-1)


type family NotZ (n :: Nat) where
    NotZ 0 = 'False
    NotZ n = 'True


class Monad m => MonadThrowJSON m where
  throw :: (ToJSON e, Exception e) => e -> m a

newtype Vec (n :: Nat) a = Vec (Int, [a]) deriving (Eq, Show, Functor, Traversable)

unVec :: Vec n a -> [a]
unVec (Vec (_,xs)) = xs

instance Foldable (Vec n) where
  foldr f b vs = foldr f b (unVec vs)
  toList = unVec

tlength :: Foldable t => t a -> Maybe SomeNat
tlength = someNatVal . toInteger . length

vec :: KnownNat n => Proxy n -> [a] -> Maybe (Vec n a)
vec p v | (fromIntegral $ natVal p) == length v = Just $ Vec (length v, v)
        | otherwise = Nothing


nil :: Vec 0 a
nil = Vec (0, [])

unconsV :: (NotZ n ~ 'True) => Vec n a -> (a , Vec (n-1) a)
unconsV (Vec (n, x:xs)) = (x, Vec (n-1, xs))
unconsV _ = error "should not be reachable"

consV :: a -> Vec n a -> Vec (n+1) a
consV x (Vec (n, xs)) = Vec (n+1, x:xs)

pattern Nil :: Vec 0 a
pattern Nil <- Vec (0, []) where
  Nil = nil

infixr :>

pattern (:>) :: NotZ n ~ 'True =>
  a -> Vec (n - 1) a -> Vec n a
pattern x :> xs <- (unconsV -> (x,xs)) where
  x :> xs = unsafeCoerce $ consV x xs

zipV :: Vec n a -> Vec n b -> Vec n (a,b)
zipV (Vec (n, as)) (Vec (_, bs)) = Vec $ (n , zip as bs)

eqLen :: Vec n a -> Vec m b -> Maybe (n :~: m)
eqLen (Vec (n, _)) (Vec (m, _)) | n == m = Just (unsafeCoerce Refl)
                     | otherwise = Nothing


