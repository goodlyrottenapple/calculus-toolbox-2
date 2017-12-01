{-|
Module      : Lib
Description : Lib's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE RecordWildCards    #-}

module Terms.JSON where

import Terms
import Lib.Prelude
import Data.Aeson
import Data.Singletons
-- import qualified Prelude as P
-- import Data.Singletons.TH
-- import GHC.TypeLits
import qualified Data.Text as T
import qualified Data.Map as M

import Text.Earley.Mixfix(Associativity(..))

newtype LatexDSeq r k = LatexDSeq { unMk :: (FinTypeCalculusDescription r, DSequent k Text) }

instance ToJSON (LatexDSeq r 'ConcreteK) where
    toJSON (LatexDSeq (d, s@(DSeq l _ r))) = object [
            "latex" .= (toJSON $ runReader (pprint s) d) -- <> " \\vdash " <> pprint r) ,
          , "term" .= toJSON s
        ]
        -- where
        --     pprint :: Term k 'ConcreteK Text -> Text
        --     pprint (Base c) = c
        --     pprint (Lift x) = pprint x
        --     pprint (Con (C c) xs) = "\\seq" <> c <> (T.intercalate "" $ map (\x -> "{" <> pprint x <> "}") $ toList xs)



getLevelT :: forall l k a. SingI l => Term l k a -> Level
getLevelT _ = fromSing (sing :: Sing l) 



getBinding :: forall l k a r m. Monad m => Term l k a -> CalcMT r m Int
getBinding (Base _) = return (maxBound :: Int)
getBinding (Meta _) = return (maxBound :: Int)
getBinding (Lift x) = getBinding x
getBinding (Con (C c) _) = do
    conns <- connMap' @l
    let ConnDescription{..} = conns M.! c
    return binding


getAssoc :: forall l k a r m. Monad m => Term l k a -> CalcMT r m Associativity
getAssoc (Base _) = return NonAssoc
getAssoc (Meta _) = return NonAssoc
getAssoc (Lift x) = return NonAssoc
getAssoc (Con (C c) _) = do
    conns <- connMap' @l
    let ConnDescription{..} = conns M.! c
    return assoc


class PPrint a where
    pprint :: Monad m => a -> CalcMT r m Text


instance PPrint Level where
    pprint AtomL = return "A"
    pprint FormulaL = return "F"
    pprint StructureL = return "S"


instance PPrint Text where
    pprint = return
instance PPrint a => PPrint (DSequent l a) where
    pprint (DSeq l _ r) = do
        pl <- pprint l
        pr <- pprint r
        return $ pl <> " \\vdash " <> pr

instance PPrint a => PPrint (Term l k a) where
    pprint t@(Meta a) = do
        l <- pprint $ getLevelT t
        pa <- pprint a
        return $ "?_{" <> l <> "} " <> pa
    pprint (Base a) = pprint a
    pprint (Lift a) = pprint a
    pprint x@(Con (C c) vs) = do
        bnd <- getBinding x
        ass <- getAssoc x
        pprint' c bnd ass $ toList vs
        where
            pprint' :: Monad m => Text -> Int -> Associativity -> [Term l k a] -> CalcMT r m Text
            pprint' c _ _ [] =  return $ "\\seq" <> c
            pprint' c bind _ [x] = do
                px <- pprint x
                bind' <- getBinding x
                return $ 
                    if bind > bind' then "\\seq" <> c <> "{(" <> px <> ")}" 
                    else "\\seq" <> c <> "{" <> px <> "}"
            pprint' c bind assoc [x,y] = do
                px <- pprint x
                py <- pprint y
                bindx <- getBinding x
                bindy <- getBinding y
                assocx <- getAssoc x
                assocy <- getAssoc y
                let left = case (compare bind bindx, assoc, assocx) of
                        (GT,_,_) -> "{(" <> px <> ")}"
                        (LT,_,_) -> "{" <> px <> "}"
                        (EQ,LeftAssoc,RightAssoc) -> "{(" <> px <> ")}"
                        (EQ,RightAssoc,RightAssoc) -> "{(" <> px <> ")}"
                        (EQ,_,_) -> "{" <> px <> "}"
                    right = case (compare bind bindy, assoc, assocy) of
                        (GT,_,_) -> "{(" <> py <> ")}"
                        (LT,_,_) -> "{" <> py <> "}"
                        (EQ,LeftAssoc,LeftAssoc) -> "{(" <> py <> ")}"
                        (EQ,RightAssoc,LeftAssoc) -> "{(" <> py <> ")}"
                        (EQ,_,_) -> "{" <> py <> "}"
                return $ "\\seq" <> c <> left <> right

            pprint' c bind _ xs = do
                pxs <- mapM pprint xs
                return $ "\\seq" <> c <> (T.intercalate "" $ map (\x -> "{" <> x <> "}") pxs)



newtype Macros = Macros (Map Text Text) deriving (Generic, ToJSON)


data CalcDesc = CalcDesc {
    name :: Text
  , rawCalc :: Text 
  , rawRules :: Text 
} deriving (Generic, ToJSON, FromJSON)
