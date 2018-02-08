{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}

module DepTerms where

-- import qualified Data.Text          as T
import           Lib.Prelude
import           Data.Map           (Map)
-- import qualified Data.Map           as M
import           Data.Set           (Set)
import qualified Data.Set           as S
-- import           Data.Singletons.TH
-- import           Data.Aeson
-- import           Text.Earley.Mixfix (Associativity)
-- import           GHC.TypeLits


data TermKind = Star | StarSet | ConcreteSet (Set Text) | Wildcard



data ConstraintTType n = ElemOf n
data TermType = ConcreteT Text -- (is of kind Star)
              | Var Int TermKind
              | ConceteSetT (Set Text) -- (is of kind StarSet)
              | FunT Text [TermType] TermKind



commaT :: Text -> Text -> TermType
commaT y ys = FunT "comma" [Var y Star, Var ys StarSet] StarSet



    -- ConstraintT :: ConstraintTType name -> TermType name -> TermType name


data DCon = DCon {
    name :: Text,
    inTypes :: [TermType],
    outType :: TermType
}


circ :: DCon
circ = DCon "circ" [Var "ys" StarSet] (commaT "y" "ys")

ex :: DCon
ex = DCon "ex" [commaT "y" "ys"] (Var "ys" StarSet)


and :: DCon
and = DCon "circ" [Var "ys" StarSet, Var "ys" StarSet] (Var "ys" StarSet)


triangle :: DCon
triangle = DCon "triangle" [Var "y" (ConcreteSet (S.fromList ["Ag", "Act", "Fnc"])), ConcreteT "Fm"] (ConcreteT "Fm")



widlcardKindVar :: Text -> TermType
widlcardKindVar x = Var x Wildcard

data DTerm = Base Text TermType
           | Lift DTerm
           | Conn DCon [DTerm]

mkBase :: Text -> Int -> DTerm
mkBase n t = Base n (Var i Star)


mkConn :: DCon -> [DTerm] -> DTerm
mkConn c ts = undefined -- unification here??

type UnficationMap = Map (Set Text) TermType




