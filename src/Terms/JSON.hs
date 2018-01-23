{-|
Module      : Terms.JSON
Description : JSON instances for the GUI

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

module Terms.JSON where

import           Data.Aeson
import           Data.Singletons
import           Lib.Prelude
import           Terms
-- import qualified Prelude as P
-- import Data.Singletons.TH
-- import GHC.TypeLits
import qualified Data.Map           as M
import qualified Data.Text          as T

import           Text.Earley.Mixfix (Associativity (..))

newtype LatexTerm r t = LatexTerm { unMk :: (FinTypeCalculusDescription r, t) }

type LatexDSeq r k = LatexTerm r (DSequent k Text)

instance (PPrint t , ToJSON t) => ToJSON (LatexTerm r t) where
    toJSON (LatexTerm (d, t)) = object [
            "latex" .= (toJSON $ runReader (pprint t) d) -- <> " \\vdash " <> pprint r) ,
          , "term" .= toJSON t
        ]


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


getAssoc :: forall l k a r m. Monad m => Term l k a -> CalcMT r m Text.Earley.Mixfix.Associativity
getAssoc (Base _) = return NonAssoc
getAssoc (Meta _) = return NonAssoc
getAssoc (Lift _) = return NonAssoc
getAssoc (Con (C c) _) = do
    conns <- connMap' @l
    let ConnDescription{..} = conns M.! c
    return assoc


class PPrint a where
    pprint :: Monad m => a -> CalcMT r m Text


instance PPrint Level where
    pprint AtomL      = return "A"
    pprint FormulaL   = return "F"
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
    pprint trm@(Con (C con) vs) = do
        bnd <- getBinding trm
        ass <- getAssoc trm
        pprint' con bnd ass $ toList vs
        where
            pprint' :: Monad m => Text -> Int -> Text.Earley.Mixfix.Associativity -> [Term l k a] -> CalcMT r m Text
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
                        (GT,_,_)                   -> "{(" <> px <> ")}"
                        (LT,_,_)                   -> "{" <> px <> "}"
                        (EQ,LeftAssoc,RightAssoc)  -> "{(" <> px <> ")}"
                        (EQ,RightAssoc,RightAssoc) -> "{(" <> px <> ")}"
                        (EQ,_,_)                   -> "{" <> px <> "}"
                    right = case (compare bind bindy, assoc, assocy) of
                        (GT,_,_)                   -> "{(" <> py <> ")}"
                        (LT,_,_)                   -> "{" <> py <> "}"
                        (EQ,LeftAssoc,LeftAssoc)   -> "{(" <> py <> ")}"
                        (EQ,RightAssoc,LeftAssoc)  -> "{(" <> py <> ")}"
                        (EQ,_,_)                   -> "{" <> py <> "}"
                return $ "\\seq" <> c <> left <> right

            pprint' c _ _ xs = do
                pxs <- mapM pprint xs
                return $ "\\seq" <> c <> (T.intercalate "" $ map (\x -> "{" <> x <> "}") pxs)



newtype Macros = Macros (Map Text Text) deriving (Generic, ToJSON)


data CalcDesc = CalcDesc {
    name     :: Text
  , rawCalc  :: Text
  , rawRules :: Text
} deriving (Generic, ToJSON, FromJSON)
