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
{-# LANGUAGE TypeInType    #-}
{-# LANGUAGE FunctionalDependencies    #-}


module Terms where

import Lib.Prelude
-- import qualified Data.Text as T

import Data.Map(Map)
import qualified Data.Map as M


import Data.Set(Set)
-- import qualified Data.Set as S


-- import TH
import GHC.TypeLits
-- import Unsafe.Coerce(unsafeCoerce)
import Data.Singletons.TH
-- import Data.Tree
-- import Data.Tree.Pretty
import Data.Aeson

-- import Data.Data

import Text.Earley.Mixfix(Associativity)


-- import Data.Vector(Vector)
-- import qualified Data.Vector as V

-- import qualified Text.Parsec.Expr as E


$(singletons [d| data Level = AtomL | FormulaL | StructureL deriving (Show, Generic, ToJSON, FromJSON) |])

deriving instance Eq Level
deriving instance Ord Level

type family Lower (l :: Level) = result | result -> l where
    Lower 'StructureL = 'FormulaL
    Lower 'FormulaL = 'AtomL

type family Raise (l :: Level) = result | result -> l where
    Raise 'FormulaL = 'StructureL
    Raise 'AtomL = 'FormulaL

type family IsAtom (l :: Level) where
    IsAtom 'AtomL = 'True
    IsAtom a = 'False

newtype Conn (l :: Level) (n :: Nat) = C Text deriving (Eq, Show)

data TermKind = ConcreteK | MetaK

data Term (l :: Level) (k :: TermKind) a where
    Base :: a -> Term 'AtomL 'ConcreteK a
    Meta :: a -> Term l 'MetaK a
    Lift :: Term (Lower l) k a -> Term l k a
    Con :: (KnownNat n, SingI l, IsAtom l ~ 'False) => -- this is a bit of a hack :/
        Conn l n -> Vec n (Term l k a) -> Term l k a 

deriving instance Show a => Show (Term l k a)


instance Eq a => Eq (Term l k a) where
    (Meta x) == (Meta y) = x == y
    (Base x) == (Base y) = x == y
    (Lift x) == (Lift y) = x == y
    (Con c1 xs) == (Con c2 ys) = case eqLen xs ys of
        Just Refl -> c1 == c2 && xs == ys
        Nothing -> False
    _ == _ = False


instance Ord a => Ord (Term 'AtomL 'MetaK a) where
    compare (Meta a) (Meta b) = compare a b
    compare _ _ = undefined

instance Ord a => Ord (Term 'FormulaL 'MetaK a) where
    compare (Meta a) (Meta b) = compare a b
    compare (Meta _) (Lift _) = LT
    compare (Meta _) (Con _ _) = LT
    compare (Lift a) (Lift b) = compare a b
    compare (Lift _) (Meta _) = GT
    compare (Lift _) (Con _ _) = LT
    compare (Con _ vs) (Con _ us) = compare (unVec vs) (unVec us)
    compare (Con _ _) (Meta _) = GT
    compare (Con _ _) (Lift _) = GT


instance Ord a => Ord (Term 'StructureL 'MetaK a) where
    compare (Meta a) (Meta b) = compare a b
    compare (Meta _) (Lift _) = LT
    compare (Meta _) (Con _ _) = LT
    compare (Lift a) (Lift b) = compare a b
    compare (Lift _) (Meta _) = GT
    compare (Lift _) (Con _ _) = LT
    compare (Con _ vs) (Con _ us) = compare (unVec vs) (unVec us)
    compare (Con _ _) (Meta _) = GT
    compare (Con _ _) (Lift _) = GT


instance Ord a => Ord (Term 'AtomL 'ConcreteK a) where
    compare (Base a) (Base b) = compare a b
    compare _ _ = undefined


instance Ord a => Ord (Term 'FormulaL 'ConcreteK a) where
    compare (Lift a) (Lift b) = compare a b
    compare (Lift _) (Con _ _) = LT
    compare (Con _ vs) (Con _ us) = compare (unVec vs) (unVec us)
    compare (Con _ _) (Lift _) = GT
 
instance Ord a => Ord (Term 'StructureL 'ConcreteK a) where
    compare (Lift a) (Lift b) = compare a b
    compare (Lift _) (Con _ _) = LT
    compare (Con _ vs) (Con _ us) = compare (unVec vs) (unVec us)
    compare (Con _ _) (Lift _) = GT
 

type MetaTerm l a = Term l 'MetaK a
type ConcreteTerm l a = Term l 'ConcreteK a




newtype CalcType = Type Text deriving (Eq, Ord, Show, Generic, ToJSON)

data ConnDescription (l :: Level) = ConnDescription {
    connName :: Text
  , inTypes :: [CalcType]
  , outType :: CalcType
  , assoc :: Associativity
  , binding :: Int
  , parserSyntax :: Text
  , latexSyntax :: Text
} deriving Show

data FinTypeCalculusDescription r = Description {
    types :: Set CalcType
  , defaultType :: CalcType
  , formulaConns :: [ConnDescription 'FormulaL]
  , structureConns :: [ConnDescription 'StructureL]
  , rules :: r
} deriving Show


data DSequent k a = DSeq (Term 'StructureL k a) CalcType (Term 'StructureL k a) deriving (Show, Eq)

deriving instance Ord a => Ord (DSequent 'MetaK a)
deriving instance Ord a => Ord (DSequent 'ConcreteK a)



instance ToJSON a => ToJSON (Vec n a) where
    toJSON = toJSON . toList


instance ToJSON a => ToJSON (Term l k a) where
    toJSON (Base c) = object [
            "Base" .= toJSON c
        ]
    toJSON (Meta x) = object [
            "Meta" .= toJSON x
        ]
    toJSON (Lift x) = object [
            "Lift" .= toJSON x
        ]
    toJSON (Con (C n) vs) = object [
            "Con" .= object [
                "name" .= toJSON n,
                "terms" .= toJSON vs
            ]
        ]

mkCon :: (IsAtom l ~ 'False, SingI l, StringConv s Text, Ord t, IsString s) =>
    Map t s -> t -> [Term l k a] -> Term l k a
mkCon d h xs = case tlength xs of
    (Just (SomeNat p)) -> let (Just vs) = vec p xs in Con (C $ toS $ M.findWithDefault "???" h d) vs
    _ -> undefined -- this can't happen...like, everrr...


unsafeMkCon :: (IsAtom l ~ 'False, SingI l) =>
    Text -> [Term l k a] -> Term l k a
unsafeMkCon c xs = case tlength xs of
    (Just (SomeNat p)) -> let (Just vs) = vec p xs in Con (C c) vs
    _ -> undefined -- this can't happen...like, everrr...



instance ToJSON a => ToJSON (DSequent k a) where
    toJSON (DSeq l (Type t) r) = object [
            "DSeq" .= object [
                "type" .= toJSON t,
                "left" .= toJSON l,
                "right" .= toJSON r
            ]
        ]


instance FromJSON a => FromJSON (Term 'AtomL 'ConcreteK a) where
    parseJSON = withObject "concrete atom term" $ \o -> Base <$> o .: "Base"

instance FromJSON a => FromJSON (Term 'FormulaL 'ConcreteK a) where
    parseJSON = withObject "concrete formula term" $ \o -> asum [
            Lift <$> o .: "Lift",
            do {
                con <- o .: "Con";
                n <- con .: "name";
                xs <- con .: "terms";
                return $ unsafeMkCon n xs
            }
        ]

instance FromJSON a => FromJSON (Term 'StructureL 'ConcreteK a) where
    parseJSON = withObject "concrete structure term" $ \o -> asum [
            Lift <$> o .: "Lift",
            do {
                con <- o .: "Con";
                n <- con .: "name";
                xs <- con .: "terms";
                return $ unsafeMkCon n xs
            }
        ]

-- instance FromJSON a => FromJSON (Term 'AtomL 'MetaK a) where
--     parseJSON = withObject "meta atom term" $ \o -> Meta <$> o .: "Meta"


-- instance FromJSON a => FromJSON (Term 'FormulaL 'MetaK a) where
--     parseJSON = withObject "meta formula term" $ \o -> asum [
--             Meta <$> o .: "Meta",
--             Lift <$> o .: "Lift",
--             do {
--                 con <- o .: "Con";
--                 n <- con .: "name";
--                 xs <- con .: "terms";
--                 return $ unsafeMkCon n xs
--             }
--         ]

-- instance FromJSON a => FromJSON (Term 'StructureL 'MetaK a) where
--     parseJSON = withObject "meta structure term" $ \o -> asum [
--             Meta <$> o .: "Meta",
--             Lift <$> o .: "Lift",
--             do {
--                 con <- o .: "Con";
--                 n <- con .: "name";
--                 xs <- con .: "terms";
--                 return $ unsafeMkCon n xs
--             }
--         ]

instance FromJSON a => FromJSON (DSequent 'ConcreteK a) where
    parseJSON = withObject "display sequent" $ \o -> do
            dseq <- o .: "DSeq"
            t <- dseq .: "type"
            l <- dseq .: "left"
            r <- dseq .: "right"
            return $ DSeq l (Type t) r

-- data Sequent k a = Seq [Term 'FormulaL k a] [Term 'FormulaL k a]


data Rule a = Rule {
    name :: Text
  , prems :: [DSequent 'MetaK a]
  , concl :: DSequent 'MetaK a
} deriving (Show, Eq, Ord, Generic, ToJSON)


type CalcMT r m = ReaderT (FinTypeCalculusDescription r) m
type CalcMErr r e = CalcMT r (Except e)
type CalcM r = CalcMT r Identity

runCalcErr :: FinTypeCalculusDescription r -> CalcMErr r e a -> Either e a
runCalcErr env = runExcept . (\rT -> runReaderT rT env)


data TypeableError a = TypeMismatch a CalcType CalcType 
                     | LevelMismatch a Level Level
                     | TypeMismatchCon Text CalcType CalcType
                       deriving (Show, Generic, ToJSON)


connMap :: MonadReader r m => (r -> [ConnDescription t]) -> m (Map Text (ConnDescription t))
connMap f = do
    fConns <- asks f
    return $ M.fromList $ map (\c@ConnDescription{..} -> (connName, c)) fConns



class TypeableCTerm l where
    typeableC' :: Ord a => Map a CalcType -> CalcType -> Term l 'ConcreteK a -> CalcMErr r (TypeableError a) (Map a CalcType)

class TypeableMTerm l where
    typeableM' :: Ord a => Map a (Level, CalcType) -> CalcType -> Term l 'MetaK a -> CalcMErr r (TypeableError a) (Map a (Level, CalcType))

typeableMeta :: (MonadError (TypeableError k) m, Ord k) => 
    Level -> Map k (Level, CalcType) -> CalcType -> k -> m (Map k (Level, CalcType))
typeableMeta l acc t a = 
        if a `M.member` acc then
            let (l', t') = acc M.! a in case (t == t', l == l') of
                (True, True) -> return $ acc
                (False, _)   -> throwError $ TypeMismatch a t t'
                (_, False)   -> throwError $ LevelMismatch a l l'
        else return $ M.insert a (l, t) acc


typeableCon :: (MonadReader d m, MonadError (TypeableError a) m) =>
     (map -> CalcType -> trm -> m map) -> (d -> [ConnDescription l]) -> map -> CalcType -> Text -> [trm] -> m map
typeableCon tyF f acc t c xs = do
    conns <- connMap f
    let ConnDescription{..} = conns M.! c 
    if t == outType then do
        let tsxs = zip inTypes xs 
        foldrM (\(t', x) acc' -> tyF acc' t' x) acc tsxs
    else throwError $ TypeMismatchCon c t outType


instance TypeableCTerm 'AtomL where
    typeableC' acc t (Base a) = if a `M.member` acc then
        let t' = acc M.! a in if t == t' then return $ acc else throwError $ TypeMismatch a t t'
        else return $ M.insert a t acc
    typeableC' _ _ (Lift _) = undefined

instance TypeableCTerm 'FormulaL where
    typeableC' acc t (Lift a) = typeableC' acc t a
    typeableC' acc t (Con (C c) vs) = typeableCon typeableC' formulaConns acc t c (unVec vs)

instance TypeableCTerm 'StructureL where
    typeableC' acc t (Lift a) = typeableC' acc t a
    typeableC' acc t (Con (C c) vs) = typeableCon typeableC' structureConns acc t c (unVec vs)



instance TypeableMTerm 'AtomL where
    typeableM' acc t (Meta a) = typeableMeta AtomL acc t a
    typeableM' _ _ (Lift _) = undefined

instance TypeableMTerm 'FormulaL where
    typeableM' acc t (Meta a) = typeableMeta FormulaL acc t a
    typeableM' acc t (Lift a) = typeableM' acc t a
    typeableM' acc t (Con (C c) vs) = typeableCon typeableM' formulaConns acc t c (unVec vs)

instance TypeableMTerm 'StructureL where
    typeableM' acc t (Meta a) = typeableMeta StructureL acc t a
    typeableM' acc t (Lift a) = typeableM' acc t a
    typeableM' acc t (Con (C c) vs) = typeableCon typeableM' structureConns acc t c (unVec vs)


typeableC :: (Ord a, TypeableCTerm l) => CalcType -> 
    Term l 'ConcreteK a -> CalcMErr r (TypeableError a) (Map a CalcType)
typeableC = typeableC' M.empty


typeableM :: (Ord a, TypeableMTerm l) => CalcType -> 
    Term l 'MetaK a -> CalcMErr r (TypeableError a) (Map a (Level, CalcType))
typeableM = typeableM' M.empty


typeableMDSeq' :: Ord a => Map a (Level, CalcType) -> DSequent 'MetaK a -> CalcMErr r (TypeableError a) (Map a (Level, CalcType))
typeableMDSeq' acc (DSeq l t r) = do
    lacc <- typeableM' acc t l
    typeableM' lacc t r


typeableCDSeq' :: Ord a => Map a CalcType -> DSequent 'ConcreteK a -> CalcMErr r (TypeableError a) (Map a CalcType)
typeableCDSeq' acc (DSeq l t r) = do
    lacc <- typeableC' acc t l
    typeableC' lacc t r



typeableRule :: Ord a => Rule a -> CalcMErr r (TypeableError a) ()
typeableRule Rule{..} = do
    conclAcc <- typeableMDSeq' M.empty concl
    _ <- foldrM (\dseq acc -> typeableMDSeq' acc dseq) conclAcc prems
    return ()


filterTypeable :: (Monad m) => 
    (t -> CalcMErr r e res) -> [t] -> CalcMT r m [t]
filterTypeable _ [] = return []
filterTypeable mf (r:rs) = do
    rs' <- filterTypeable mf rs
    env <- ask
    case runCalcErr env $ mf r of
        Left _ -> return rs'
        Right _ -> return $ r:rs'