{-
Welcome to your custom Prelude
Export here everything that should always be in your library scope
For more info on what is exported by Protolude check:
https://github.com/sdiehl/protolude/blob/master/Symbols.md
-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ScopedTypeVariables   #-}


module Lib.Prelude
    ( module Exports, MonadThrowJSON(..), SomeVec(..)
      , Length, Vec, vec, Copy, NotZ, pattern Nil, pattern (:>), eqLen, zipV, tlength
    ) where

import           GHC.TypeLits
import           Protolude     as Exports
import           Unsafe.Coerce (unsafeCoerce)

import           Data.Aeson
-- import           GHC.TypeNats (someNatVal)

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

-- tlength :: Foldable t => t a -> SomeNat
-- tlength = GHC.TypeNats.someNatVal . toEnum . length

tlength :: Foldable t => t a -> Maybe SomeNat
tlength = someNatVal . toInteger . length


data SomeVec a = forall n. KnownNat n => SomeVec (Vec n a)

vec :: [a] -> SomeVec a
vec xs = case tlength xs of
  Just (SomeNat (_ :: Proxy n)) -> SomeVec $ Vec @n (length xs, xs)
  Nothing -> undefined


-- nil :: Vec 0 a
-- nil = Vec (0, [])

unconsV :: (NotZ n ~ 'True) => Vec n a -> (a , Vec (n-1) a)
unconsV (Vec (n, x:xs)) = (x, Vec (n-1, xs))
unconsV _ = undefined


pattern Nil :: Vec 0 a
pattern Nil <- Vec (0, []) where
  Nil = Vec (0, [])

infixr :>

pattern (:>) :: NotZ n ~ 'True =>
  a -> Vec (n - 1) a -> Vec n a
pattern x :> xs <- (unconsV -> (x,xs)) where
  x :> xs = unsafeCoerce $ consV x xs
    where
      consV :: a -> Vec n a -> Vec (n+1) a
      consV y (Vec (n, ys)) = Vec (n+1, y:ys)


zipV :: Vec n a -> Vec n b -> Vec n (a,b)
zipV (Vec (n, as)) (Vec (_, bs)) = Vec $ (n , zip as bs)

eqLen :: Vec n a -> Vec m b -> Maybe (n :~: m)
eqLen (Vec (n, _)) (Vec (m, _)) | n == m = Just (unsafeCoerce Refl)
                     | otherwise = Nothing

instance ToJSON a => ToJSON (Vec n a) where
    toJSON = toJSON . toList


