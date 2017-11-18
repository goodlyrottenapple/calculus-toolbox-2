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
-- import qualified Prelude as P
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


type MetaTerm l a = Term l 'MetaK a
type ConcreteTerm l a = Term l 'ConcreteK a


unify :: (Ord a, Ord b) => MetaTerm l a -> ConcreteTerm l b -> Maybe (Map a (ConcreteTerm l b))
unify (Meta x) y = Just $ M.singleton x y
unify (Lift lx) (Lift ly) = case unify lx ly of
    Just u ->  Just $ M.map (\t -> Lift t) u
    Nothing -> Nothing

unify (Con (C c1) vs) (Con (C c2) us) = case eqLen vs us of
    Just Refl -> if c1 == c2 then unifyC vs us else Nothing
    Nothing -> Nothing

    where
        unifyC :: (Ord a, Ord b) => 
            Vec n (MetaTerm l a) -> Vec n (ConcreteTerm l b) -> Maybe (Map a (ConcreteTerm l b))
        unifyC xs ys = 
            foldr unifyFold (Just $ M.empty) (zipV xs ys)

        unifyFold _ Nothing = Nothing
        unifyFold (x,y) (Just ws) = do
            u <- unify x y
            let uInteresction = M.intersection u ws
                c = M.foldrWithKey (\k v b -> checkConsistent k v ws && b) True uInteresction
            if c then return $ M.union u ws else Nothing

        checkConsistent :: (Ord k, Eq v) => k -> v -> Map k v -> Bool
        checkConsistent k v1 m | (Just v2) <- M.lookup k m = v1 == v2
                               | otherwise = False
unify _ _ = Nothing


sub :: Ord a => Map a (ConcreteTerm l b) -> MetaTerm l a -> Maybe (ConcreteTerm l b)
sub m (Meta x) = M.lookup x m
sub m (Lift lx) = do
    let m' = fromMaybe M.empty $ traverse pickLifted m
    t <- sub m' lx
    return $ Lift t
    where
        pickLifted (Lift x) = Just x
        pickLifted _ = Nothing
sub m (Con (C c) xs) = do
    xs' <- traverse (sub m) xs
    return $ Con (C c) xs'



newtype CalcType = Type Text deriving (Eq, Ord, Show)

data ConnDescription (l :: Level) = ConnDescription {
    connName :: Text
  , inTypes :: [CalcType]
  , outType :: CalcType
  , assoc :: Associativity
  , binding :: Int
  , parserSyntax :: Text
  -- , latexSyntax :: Text
} deriving Show

data FinTypeCalculusDescription r = Description {
    types :: Set CalcType
  , defaultType :: CalcType
  , formulaConns :: [ConnDescription 'FormulaL]
  , structureConns :: [ConnDescription 'StructureL]
  , rules :: r
} deriving Show


data DSequent k a = DSeq (Term 'StructureL k a) CalcType (Term 'StructureL k a) deriving Show

-- data Sequent k a = Seq (Term 'FormulaL k a) (Term 'FormulaL k a)


data Rule a = Rule {
    name :: Text
  , prems :: [DSequent 'MetaK a]
  , concl :: DSequent 'MetaK a
} deriving Show


type CalcMT r m = ReaderT (FinTypeCalculusDescription r) m
type CalcMErr r e = CalcMT r (Except e)
type CalcM r = CalcMT r Identity

runCalcErr :: FinTypeCalculusDescription r -> CalcMErr r e a -> Either e a
runCalcErr env = runExcept . (\rT -> runReaderT rT env)


data TypeableError a = TypeMismatch a CalcType CalcType 
                     | LevelMismatch a Level Level
                     | TypeMismatchCon Text CalcType CalcType 
                       deriving Show


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
