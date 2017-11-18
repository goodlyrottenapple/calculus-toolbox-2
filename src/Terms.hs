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




-- typeable' acc t trm = undefined


-- class TyOf v t | v -> t where
--     tyOf :: Term v l k a -> Maybe t

-- -- instance TyOf v () where
-- --     tyOf _ = Just ()




-- newtype ConMap (v :: Symbol) t = ConMap {unwrap :: Map (Text, Level) ([t] , t)}


-- class FinTypeCon (v :: Symbol) where
--     conTys :: ConMap v (FiniteType v) 

-- data family FiniteType (v :: Symbol)

-- data instance FiniteType "DEAK" = AtpropD | AgentD
-- deriving instance Eq (FiniteType "DEAK")
-- deriving instance Show (FiniteType "DEAK")
-- deriving instance Typeable (FiniteType "DEAK")
-- deriving instance Data (FiniteType "DEAK")


-- instance FinTypeCon "DEAK" where
--     conTys = ConMap $ M.fromList [(("and", FormulaL), ([AtpropD,AtpropD], AtpropD))]


-- getConTy :: forall v l n. (KnownNat n, SingI l , FinTypeCon v) => Conn v l n -> (Vec n (FiniteType v), (FiniteType v))
-- getConTy c@(C t) = (v , snd x)
--     where
--         x = case M.lookup (t, getLevel c) (unwrap $ conTys @v) of
--             Just v -> v
--             Nothing -> error "The connecive is not defined, please check the FinTypeConD instance"
--         v = case vec (Proxy :: Proxy n) $ fst x of
--             Just x -> x
--             Nothing -> error "The defined connective has the wrong number of arguments, please check the FinTypeConD instance"

--         getLevel :: forall v l n. SingI l => Conn v l n -> Level
--         getLevel _ = fromSing (sing :: Sing l) 


-- instance (Eq (FiniteType v) , FinTypeCon v) => TyOf v (Either (FiniteType v) ()) where
--     -- tyOf :: forall t v l k a. (Eq t , FinTypeCon v t) => Term v l k a -> Maybe (FinType t)
--     tyOf (Base _) = Just $ Right ()
--     tyOf (Meta _) = Just $ Right ()
--     tyOf (Lift x) = tyOf x
--     tyOf (Con c xs) = do
--         let (cts, ct) = getConTy c
--         ts <- mapM tyOf xs
--         case eqLen cts ts of
--             Just Refl -> if ts ~~ cts then return $ Left ct else Nothing
--             Nothing -> Nothing

--         where
--             (~~) :: Vec n (Either (FiniteType v) ()) -> Vec n (FiniteType v) -> Bool
--             ets ~~ ts = foldr (\(et, t) b -> equiv et t && b) True $ zipV ets ts

--             equiv (Right _) _ = True
--             equiv (Left s) t = s == t





-- data family FiniteTypeCons (vOfCalc :: Symbol)
--                            (level :: Level) 
--                            (ts :: [k])
--                            (t :: k)


-- $(singletons [d| data DEAKTypes = Atprop | Agent deriving Show |])

-- data instance FiniteTypeCons "DEAK" l ts t where
--     And :: FiniteTypeCons "DEAK" 'FormulaL '[Atprop, Atprop] Atprop


-- getTypes :: forall k ts v l t. (SingKind k , SingI ts, KnownNat (Length ts)) => 
--     FiniteTypeCons v l (ts :: [k]) t -> Vec (Length ts) (DemoteRep k)
-- getTypes _ = let (Just x) = vec (Proxy :: Proxy (Length ts)) $ fromSing (sing :: Sing ts) in x

-- getType :: forall k ts v l t. (SingKind k , SingI t) => FiniteTypeCons v l (ts :: [k]) t -> DemoteRep k
-- getType _ = fromSing (sing :: Sing t) 




-- instance FinTypeCon "DEAK" DEAKTypes where
--     getConTy (C "and") = ((Atprop @@ (Atprop @@ nil)) , Atprop)


-- instance (Eq t , FinTypeCon v t) => TyOf v (FinType t) where
--     -- tyOf :: forall t v l k a. (Eq t , FinTypeCon v t) => Term v l k a -> Maybe (FinType t)
--     tyOf (Base _) = Just $ FinType $ Right ()
--     tyOf (Meta _) = Just $ FinType $ Right ()
--     tyOf (Lift x) = tyOf x
--     tyOf (Con c xs) = do
--         let (cts, ct) = getConTy c
--         ts <- mapM tyOf xs
--         case eqLen cts ts of
--             Just Refl -> if ts ~~ cts then return $ FinType $ Left ct else Nothing
--             Nothing -> Nothing
--         Nothing

--         where
--             (~~) :: Eq t => Vec n (FinType t) -> Vec n t -> Bool
--             ets ~~ ts = foldr (\(et, t) b -> equiv et t && b) True $ zipV ets ts

--             equiv (FinType (Right _)) _ = True
--             equiv (FinType (Left s)) t = s == t


-- -- newtype FinType (v :: Symbol) (l :: Level) (n :: Nat) a where
-- --     FinType :: FinTypeCon v l n a => Either a () -> FinType v l n a





--     
            


-- data TypeKind = SingleTK | FiniteTK | UnrestrictedTK


-- class WellTyped (tk :: TypeKind) v l k a where
--     wellTy :: Proxy tk -> Term v l k a -> Bool


-- instance WellTyped 'SingleTK  v l k a where
--     wellTy _ _ = True

-- class FiniteConnTy v l n t where 
--     conTyOf :: Conn (v :: Symbol) (l :: Level) (n :: Nat) -> ([t] , t)



-- instance FiniteConnTy v l n t => WellTyped 'FiniteTK where
--     wellTy _ _ = True

-- finTyOf :: FiniteConnTy v l n t => Term v l k a -> Maybe t

-- wrapper for a single type display calculus
-- newtype SType (k :: TermKind) = S () deriving Eq

-- instance ConT v l n k SType where
--     conType _ _ = Just $ S ()




-- and :: Conn "DEAK" 'FormulaL k 2 (FType t)
-- and = appType 



-- instance ConT v l n k (FType a) where
--     conType _ _ = Just $ S ()


-- tyOf :: Term v l k t a -> Maybe (t k)
-- tyOf (Base t _) = Just t
-- tyOf (Meta t _) = Just t
-- tyOf (Lift x) = tyOf x
-- tyOf (Con c xs) = do
--     ts <- mapM tyOf xs
--     conType c ts




-- -- This is weaker than syntactic equality. It might be useful when defining types for DFOL,
-- -- since at the meta-level, we dont want concrete types, but type holes, eg if we have a connective
-- -- that introduces a fresh variable, we want to esentailly give the meta variable any type and only check the type
-- -- once weve unified and substituted...
-- class Equiv t u where
--     (~~) :: t -> u -> Bool


-- -- instance Eq a => Equiv a a where
-- --     a ~~ b = a == b

-- instance Equiv a b => Equiv (Maybe a) (Maybe b) where
--     (Just a) ~~ (Just b) = a ~~ b
--     Nothing ~~ Nothing = True
--     _ ~~ _ = False


-- instance Equiv (t k1) (t k2) => Equiv (Conn v l k1 n t) (Conn v l k2 n t) where
--     (C x) ~~ (C y) = x == y

-- -- instance Equiv s t => Equiv (Conn v l k n s) (Conn v l k' n t) where
-- --     (C x) ~~ (C y) = x == y




-- data Seq (v :: Symbol) (k :: TermKind) t a = 
--     Seq (Term v 'StructureL k t a) (Term v 'StructureL k t a)


-- -- data MSequent (v :: Symbol) t a = 
-- --     MSeq (MTerm v 'StructureL t a) (MTerm v 'StructureL t a)



-- wfSeq :: Eq (t k) => Terms.Seq v k t a -> Bool
-- wfSeq (Seq l r) = tyOf l == tyOf r 



-- -- fresh :: (Ord a) => Connective "DFOL" 'FormulaL 2 (Set a)
-- -- fresh = Conn {
-- --     name = "Fresh"
-- --   , typ = appType $ \x xs -> if S.size x == 1 then Just $ x `S.union` xs else Nothing
-- --   , parserSyntax = "*_"
-- -- }



-- -- wrapper for a display calculus with finite types
-- newtype FType a (k :: TermKind) = F a deriving (Eq, Show)

-- instance Eq a => Equiv (FType a k1) (FType a k2) where
--     (F a) ~~ (F b) = a == b







-- data DEAKType = Atprop | Agent deriving (Eq, Show)

-- -- and :: (FType DEAKType) -> (FType DEAKType) -> Maybe (FType DEAKType)
-- and (F Atprop) (F Atprop) = Just $ F Atprop
-- and _ _ = Nothing

-- box (F Agent) (F Atprop) = Just $ F Atprop
-- box _ _ = Nothing

-- instance ConT "DEAK" 'FormulaL 2 k (FType DEAKType) where
--     conType (C "and") = appType Terms.and
--     conType (C "box") = appType Terms.box
--     conType _ = \_ -> Nothing








-- data DFOL a (k :: TermKind) where
--     Hole :: DFOL a 'MetaK
--     CType :: a -> DFOL a 'ConcreteK


-- instance Equiv (DFOL a 'MetaK) (DFOL a 'ConcreteK) where
--     Hole ~~ (CType _) = True

-- freshC :: Ord a => DFOL (Set a) 'ConcreteK -> DFOL (Set a) 'ConcreteK -> Maybe (DFOL (Set a) 'ConcreteK)
-- freshC (CType x) (CType xs) = if length x == 1 then Just $ CType $ x `S.union` xs else Nothing

-- freshM :: DFOL a 'MetaK -> DFOL a 'MetaK -> Maybe (DFOL a 'MetaK)
-- freshM _ _ = Just $ Hole


-- instance Ord a => ConT "DFOL" 'FormulaL 2 'ConcreteK (DFOL (Set a)) where
--     conType (C "fresh") = appType freshC
--     conType _ = \_ -> Nothing

-- instance ConT "DFOL" 'FormulaL 2 'MetaK (DFOL a) where
--     conType (C "fresh") = appType freshM
--     conType _ = \_ -> Nothing






-- -- data UTTerm (v :: Symbol) (l :: Level) (k :: TermKind) a where
-- --     UBase :: a -> UTTerm v 'AtomL 'ConcreteK a
-- --     UMeta :: a -> UTTerm v l 'MetaK a
-- --     ULift :: UTTerm v (Lower l) k a -> UTTerm v l k a
-- --     UCon :: Text -> [UTTerm v l k a] -> UTTerm v l k a 

-- deriving instance Show a => Show (UTTerm v l k a)



-- class ConTOp (v :: Symbol) (l :: Level) (k :: TermKind) t where
--     conTypeOp :: Conn (v :: Symbol) (l :: Level) (k :: TermKind) (n :: Nat) t -> t k -> Maybe (Vec n (t k))




-- class TypeInf v l k t a where
--     inferTy :: t k -> UTTerm v l k a -> Maybe (Term v l k t a)


-- instance TypeInf v l k SType a where
--     inferTy _ (UBase a) = Just $ Base (S ()) a
--     inferTy _ (UMeta a) = Just $ Meta (S ()) a
--     inferTy t (ULift x) = map Lift $ inferTy t x
--     inferTy t (UCon c xs) = do
--         xs' <- mapM (inferTy t) xs
--         SomeNat p <- someNatVal $ toInteger $ length xs'
--         vs <- vec p xs'
--         let t = Con (C c) vs
--         _ <- tyOf t
--         return $ t




-- instance ConTOp v l k (FType x) => TypeInf v l k (FType x) a where
--     inferTy t (UBase a) = Just $ Base t a
--     inferTy t (UMeta a) = Just $ Meta t a
--     inferTy t (ULift x) = map Lift $ inferTy t x
--     inferTy t (UCon c xs) = do
--         SomeNat p <- someNatVal $ toInteger $ length xs
--         vs <- vec p xs
--         let con = C c
--         ts <- conTypeOp con t
--         case eqLen vs ts of
--             Just Refl -> do
--                 let zs = zipV ts vs 
--                 vs' <- mapM (\(t,v) -> inferTy t v) zs
--                 return $ Con con vs'
--             Nothing -> Nothing



-- instance WellFormed CTerm where
--     wfTerm (BaseC _ _) = True
--     wfTerm (LiftC t) = wfTerm t
--     wfTerm t@(ConC Conn{..} ts) = isJust $ tyOfTerm t --&& 
--     -- foldr (\(t , x) rest -> wfTerm x && tyOfTerm x == t && rest) True $ zip args ts


-- instance WellFormed MTerm where
--     wfTerm (MetaM _ _) = True
--     wfTerm (LiftM t) = wfTerm t
--     wfTerm (ConM Conn{..} ts) | length args == length ts = foldr (\(t , x) rest -> wfTerm x && tyOfTerm x == t && rest) True $ zip args ts
--                               | otherwise = False




-- data Term (v :: Symbol) (l :: Level) (isMeta :: Bool) (mtype :: k) a where
--     MetaV :: SingI l => a -> Term v l 'True mtype a
--     Base :: a -> Term v 'AtomL 'False mtype a
--     Lift :: SingI c => Term v (Lower c) m mtype a -> Term v c m mtype a
--     Coerce :: Eq (DemoteRep k) => Term v c 'True (mtype :: k) a -> Term v c 'True (mtype1 :: k) a
--     -- HomC :: (Eq (HomCon v c n f p b l) , KnownNat n, KnownSymbol p, SingI f, KnownNat b) => 
--     --     HomCon v c n f p b l -> Vec n (Term v c m mtype a) -> Term v c m mtype a
--     Con :: Eq (HetCon v mtype c args f p b l) =>
--         HetCon v mtype c args f p b l -> THList v c m args a -> Term v c m mtype a


-- deriving instance Functor (THList v l 'True as)
-- deriving instance Functor (THList v l 'False as)

-- instance Functor (Term v l 'True mtype) where
--     fmap f (MetaV x) = MetaV $ f x
--     fmap f (Lift x) = Lift $ map f x
--     fmap f (Coerce x) = Coerce $ map f x
--     fmap f (Con c xs) = Con c $ (map f xs)

-- instance Functor (Term v l 'False mtype) where
--     fmap f (Base x) = Base $ f x
--     fmap f (Lift x) = Lift $ map f x
--     fmap f (Con c xs) = Con c $ (map f xs)


-- -- deriving instance Show a => Show (Term v l m a)

-- -- deriving 


-- instance Eq a => Eq (THList v l m as a) where
--     Nil == Nil = True
--     (x :> xs) == y :> ys = x == y && xs == ys

-- instance Eq a => Eq (Term v l m mtype a) where
--     (MetaV x) == (MetaV y) = x == y
--     (Base x) == (Base y) = x == y
--     (Lift x) == (Lift y) = x == y
--     -- (Coerce d1 x) == (Coerce d2 y) = d1 == d2 && x == (unsafeCoerce y)
--     (Con c1 xs) == (Con c2 ys) = 
--         -- we should be able to get away witht this, beacuse Haskell is lazy
--         -- and if c1 /= c2, then the experssion evaluates to False and we won't do the coersion of ys.
--         -- however, if c1 == c2, then we should be able to safely coerce ys
--         c1 == (unsafeCoerce c2) && xs == (unsafeCoerce ys)
--     _ == _ = False

-- instance Ord a => Ord (Term v l m mtype a) where
--     compare (MetaV x) (MetaV y) = compare x y
--     compare (Base x) (Base y) = compare x y
--     compare (Lift x) (Lift y) = compare x y
--     -- compare (Con _ x) (Con _ y) = compare (length x) (length y)
--     -- compare _ _ = EQ


-- type family H (t :: k) (n :: Nat) where
--     H t 0 = '[]
--     H t 1 = '[t]
--     H t 2 = '[t, t]
--     H t 3 = '[t, t, t]
--     H t 4 = '[t, t, t, t]
--     H t 5 = '[t, t, t, t, t]

-- type H' n = H () n

-- $(singletons [d| data S = S deriving Eq |])


-- data instance HetCon "LK" m l as f p b ltx where
--     CommaLK :: HetCon "LK" 'S 'StructureL (H 'S 2) 'InfixL             ";"     1 '("comma" , "\\color{blue}{{#1}}")
--     AndLK ::   HetCon "LK" 'S 'FormulaL   (H 'S 2) 'InfixL             "/\\"   1 '("and" , "\\color{blue}{{#1}}")
--     -- OrLKC ::    HomCon "LK" 'FormulaL   2 'InfixL             "\\/"   1 '("or" , "\\color{blue}{{#1}}")
--     -- ImpLKC ::   HomCon "LK" 'FormulaL   2 'InfixL             "->"    1 '("impl" , "\\color{blue}{{#1}}")
--     -- NegLKC ::   HomCon "LK" 'FormulaL   1 'Terms.Prefix        "~"    1 '("neg" , "\\color{blue}{{#1}}")
--     -- Box ::      HomCon "LK" 'FormulaL   2 'Terms.Prefix "[]"    1 '("comma" , "\\color{blue}{{#1}}")
-- deriving instance Eq (HetCon "LK" m l n f p b ltx)



-- $(singletons [d| data Types = Atprop | Agent deriving Eq |])

-- data instance HetCon "DEAK" m l as f p b ltx where
--     CommaDEAK :: HetCon "DEAK" 'Atprop 'StructureL (H 'Atprop 2) 'InfixL             ";"     1 '("comma" , "\\color{blue}{{#1}}")
--     AndDEAK ::   HetCon "DEAK" 'Atprop 'FormulaL   (H 'Atprop 2) 'InfixL             "/\\"   1 '("and" , "\\color{blue}{{#1}}")
--     -- OrLKC ::    HomCon "LK" 'FormulaL   2 'InfixL             "\\/"   1 '("or" , "\\color{blue}{{#1}}")
--     -- ImpLKC ::   HomCon "LK" 'FormulaL   2 'InfixL             "->"    1 '("impl" , "\\color{blue}{{#1}}")
--     -- NegLKC ::   HomCon "LK" 'FormulaL   1 'Terms.Prefix        "~"    1 '("neg" , "\\color{blue}{{#1}}")
--     -- Box ::      HomCon "LK" 'FormulaL   2 'Terms.Prefix "[]"    1 '("comma" , "\\color{blue}{{#1}}")
-- deriving instance Eq (HetCon "DEAK" m l n f p b ltx)





-- type SingleTypeSeq v m a = MultiTypeSeq v () m a

-- type MetaSeq v t a = MultiTypeSeq v t 'True a


-- pattern (:|-:) :: forall (v :: Symbol) a m. Term v 'StructureL m a -> Term v 'StructureL m a -> SingleTypeSeq v m a
-- pattern a :|-: b = Seq a () b






-- type At v a = Term v 'AtomL a

-- type Form v a = Term v 'FormulaL a



-- -- pattern MV a = MetaV (Terms.Meta a)


-- data SRule (v :: Symbol) mtype a b = SRule {
--     prems :: [MetaSeq v mtype a],
--     concl :: MetaSeq v mtype b
-- }

-- type HomRule v mtype a = SRule v mtype a a


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


-- data ParserDataC = ParserDataC {
--     parserSymbol :: Text
--   , noOfArgs :: Int
--   , fixity :: Terms.Fixity
--   , binding :: Int
-- } deriving Show


-- mkParserDataC :: forall name l n as p b ltx. 
--     (KnownSymbol p, KnownNat n, SingI as, KnownNat b) => 
--     HomCon name l n as p b ltx -> ParserDataC
-- mkParserDataC c = ParserDataC{..}
--     where
--         parserSymbol = getParserSymbol c
--         noOfArgs = getNoOfArgs c
--         fixity = getFixity c
--         binding = getBinding c



-- class ParserData v where
--     getParserData :: Proxy v -> [ParserDataC]

