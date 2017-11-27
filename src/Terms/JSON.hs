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
-- import qualified Prelude as P
-- import Data.Singletons.TH
-- import GHC.TypeLits
import qualified Data.Text as T


newtype LatexDSeq r k = LatexDSeq { unMk :: (FinTypeCalculusDescription r, DSequent k Text) }

instance ToJSON (LatexDSeq r 'ConcreteK) where
    toJSON (LatexDSeq (_, s@(DSeq l _ r))) = object [
            "latex" .= (toJSON $ pprint l <> " \\vdash " <> pprint r) ,
            "term" .= toJSON s
        ]
        where
            pprint :: Term k 'ConcreteK Text -> Text
            pprint (Base c) = c
            pprint (Lift x) = pprint x
            pprint (Con (C c) xs) = "\\seq" <> c <> (T.intercalate "" $ map (\x -> "{" <> pprint x <> "}") $ unVec xs)


newtype Macros = Macros (Map Text Text) deriving (Generic, ToJSON)


data CalcDesc = CalcDesc {
    name :: Text
  , rawCalc :: Text 
  , rawRules :: Text 
} deriving (Generic, ToJSON, FromJSON)

-- -- calcName :: forall name l n as p b. KnownSymbol name => HomCon name l n as p b -> Text
-- -- calcName _ = toS $ symbolVal (Proxy :: Proxy name)


-- type family Fst (t :: (k,k)) where
--     Fst '(x,y) = x

-- type family Snd (t :: (k,k)) where
--     Snd '(x,y) = y

-- getParserSymbol :: forall name l n as p b ltx. KnownSymbol p => HomCon name l n as p b ltx -> Text
-- getParserSymbol _ = toS $ symbolVal (Proxy :: Proxy p)

-- getNoOfArgs :: forall name l n as p b ltx. KnownNat n => HomCon name l n as p b ltx -> Int
-- getNoOfArgs _ = fromIntegral $ natVal (Proxy :: Proxy n)

-- getFixity :: forall name l n as p b ltx. SingI as => HomCon name l n as p b ltx -> Terms.Fixity
-- getFixity _ = fromSing (sing :: Sing as)

-- getLevelT :: forall v l m a. SingI l => Term v l m a -> Level
-- getLevelT _ = fromSing (sing :: Sing l) 

-- getBinding :: forall name l n as p b ltx. KnownNat b => HomCon name l n as p b ltx -> Int
-- getBinding _ = fromIntegral $ natVal (Proxy :: Proxy b)

-- latexName :: forall name l n as p b ltx. KnownSymbol (Fst ltx) => HomCon name l n as p b ltx -> Text
-- latexName _ = toS $ symbolVal (Proxy :: Proxy (Fst ltx))

-- latexDef :: forall name l n as p b ltx. KnownSymbol (Snd ltx) => HomCon name l n as p b ltx -> Text
-- latexDef _ = toS $ symbolVal (Proxy :: Proxy (Snd ltx))


-- class PPrint a where
--     pprint :: a -> Text
--     mkTree :: a -> Tree Text


-- instance PPrint Char where
--     pprint x = toS [x]
--     mkTree x = Node (pprint x) []

-- instance PPrint Text where
--     pprint = identity 
--     mkTree x = Node x []

-- instance KnownSymbol p => PPrint (HomCon name l n as p b ltx) where
--     pprint c = getParserSymbol c
--     mkTree = undefined

-- instance PPrint Level where
--     pprint AtomL = "A"
--     pprint FormulaL = "F"
--     pprint StructureL = "S"
--     mkTree = undefined



-- instance (PPrint a) => PPrint (Term v l m a) where
--     pprint t@(MetaV a) = "?" <> (pprint $ getLevelT t) <> " " <> pprint a
--     pprint (Base a) = pprint a
--     pprint (Lift a) = pprint a
--     pprint (HomC c Nil) = pprint c
--     pprint (HomC c (x@(HomC _ _) :> Nil)) = pprint c <> " (" <> pprint x <> ")"
--     pprint (HomC c (x :> Nil)) = pprint c <> " " <> pprint x
--     pprint (HomC c (x@(HomC _ _) :> y :> Nil)) | getFixity c == Terms.InfixL = pprint x <> " " <> pprint c <> " " <> pprint y
--     pprint (HomC c (x@(HomC _ _) :> y :> Nil)) | getFixity c == Terms.InfixR = "(" <> pprint x <> ")" <> " " <> pprint c <> " " <> pprint y
--     pprint (HomC c (x :> y@(HomC _ _) :> Nil)) | getFixity c == Terms.InfixR = pprint x <> " " <> pprint c <> " " <> pprint y
--     pprint (HomC c (x :> y@(HomC _ _) :> Nil)) | getFixity c == Terms.InfixL = pprint x <> " " <> pprint c <> " " <> "(" <> pprint y <> ")"
--     pprint (HomC c (x :> y :> Nil)) | getFixity c /= Terms.Prefix = pprint x <> " " <> pprint c <> " " <> pprint y
--     pprint (HomC c ts) = (pprint c) <> (T.intercalate " " $ toList $ map pprint ts)

--     mkTree t@(MetaV a) = Node "?" [mkTree a]
--     mkTree (Base a) = Node (pprint a) []
--     mkTree t@(Lift a) = Node (pprint $ getLevelT t) [mkTree a]
--     mkTree (HomC c ts) = Node (pprint c) (map mkTree $ toList ts)


-- pprintTree :: PPrint a => a -> Text
-- pprintTree a = toS $ drawVerticalTree $ map toS $ mkTree a

-- class MkHomCon v c where
--     mkHomCon :: Text -> [Term v c m a] -> Maybe (Term v c m a)