{-|
Module      : Lib
Description : Lib's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE FlexibleContexts          #-}
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

module Terms where

import qualified Data.Text          as T
import           Lib.Prelude
import           Data.Map           (Map)
import qualified Data.Map           as M
import           Data.Set           (Set)
import           Data.Singletons.TH
import           Unsafe.Coerce      (unsafeCoerce)
import           Data.Aeson
import           Text.Earley.Mixfix (Associativity)
import           GHC.TypeLits


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
    Meta :: SingI l => a -> Term l 'MetaK a
    Lift :: SingI l => Term (Lower l) k a -> Term l k a
    Con :: (KnownNat n, SingI l, IsAtom l ~ 'False) => -- this is a bit of a hack :/
        Conn l n -> Vec n (Term l k a) -> Term l k a

deriving instance Show a => Show (Term l k a)


instance Eq a => Eq (Term l k a) where
    (Meta x) == (Meta y) = x == y
    (Base x) == (Base y) = x == y
    (Lift x) == (Lift y) = x == y
    (Con c1 xs) == (Con c2 ys) = case eqLen xs ys of
        Just Refl -> c1 == c2 && xs == ys
        Nothing   -> False
    _ == _ = False


instance Ord a => Ord (Term 'AtomL 'MetaK a) where
    compare (Meta a) (Meta b) = compare a b
    compare _ _ = undefined

instance Ord a => Ord (Term 'FormulaL 'MetaK a) where
    compare (Meta a) (Meta b)     = compare a b
    compare (Meta _) (Lift _)     = LT
    compare (Meta _) (Con _ _)    = LT
    compare (Lift a) (Lift b)     = compare a b
    compare (Lift _) (Meta _)     = GT
    compare (Lift _) (Con _ _)    = LT
    compare (Con _ vs) (Con _ us) = compare (toList vs) (toList us)
    compare (Con _ _) (Meta _)    = GT
    compare (Con _ _) (Lift _)    = GT


instance Ord a => Ord (Term 'StructureL 'MetaK a) where
    compare (Meta a) (Meta b)     = compare a b
    compare (Meta _) (Lift _)     = LT
    compare (Meta _) (Con _ _)    = LT
    compare (Lift a) (Lift b)     = compare a b
    compare (Lift _) (Meta _)     = GT
    compare (Lift _) (Con _ _)    = LT
    compare (Con _ vs) (Con _ us) = compare (toList vs) (toList us)
    compare (Con _ _) (Meta _)    = GT
    compare (Con _ _) (Lift _)    = GT


instance Ord a => Ord (Term 'AtomL 'ConcreteK a) where
    compare (Base a) (Base b) = compare a b
    compare _ _ = undefined


instance Ord a => Ord (Term 'FormulaL 'ConcreteK a) where
    compare (Lift a) (Lift b)     = compare a b
    compare (Lift _) (Con _ _)    = LT
    compare (Con _ vs) (Con _ us) = compare (toList vs) (toList us)
    compare (Con _ _) (Lift _)    = GT

instance Ord a => Ord (Term 'StructureL 'ConcreteK a) where
    compare (Lift a) (Lift b)     = compare a b
    compare (Lift _) (Con _ _)    = LT
    compare (Con _ vs) (Con _ us) = compare (toList vs) (toList us)
    compare (Con _ _) (Lift _)    = GT


type MetaTerm l a = Term l 'MetaK a
type ConcreteTerm l a = Term l 'ConcreteK a




newtype CalcType = Type Text deriving (Eq, Ord, Show, Generic, ToJSON)

data ConnDescription (l :: Level) = ConnDescription {
    name         :: Text
  , inTypes      :: [CalcType]
  , outType      :: CalcType
  , assoc        :: Text.Earley.Mixfix.Associativity
  , binding      :: Int
  , parserSyntax :: Text
  , latexSyntax  :: Text
} deriving Show

data FinTypeCalculusDescription r = Description {
    types          :: Set CalcType
  , defaultType    :: CalcType
  , formulaConns   :: [ConnDescription 'FormulaL]
  , structureConns :: [ConnDescription 'StructureL]
  , rules          :: r
} deriving Show


data DSequent k a = DSeq (Term 'StructureL k a) CalcType (Term 'StructureL k a) deriving (Show, Eq)

deriving instance Ord a => Ord (DSequent 'MetaK a)
deriving instance Ord a => Ord (DSequent 'ConcreteK a)


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
mkCon d h xs = case vec xs of
    (SomeVec vs) -> Con (C $ toS $ M.findWithDefault "???" h d) vs


unsafeMkCon :: (IsAtom l ~ 'False, SingI l) =>
    Text -> [Term l k a] -> Term l k a
unsafeMkCon c xs = case vec xs of
    (SomeVec vs) -> Con (C c) vs



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


data Rule a = Rule Text [DSequent 'MetaK a] (DSequent 'MetaK a)
            | RevRule Text (DSequent 'MetaK a) (DSequent 'MetaK a)
              deriving (Show, Eq, Ord, Generic, ToJSON)


ruleName :: Rule a -> Text
ruleName (Rule n _ _) = n
ruleName (RevRule n _ _) = n

type CalcMT r m = ReaderT (FinTypeCalculusDescription r) m


data TypeableError a = TypeMismatch a CalcType CalcType
                     | LevelMismatch a Level Level
                     | forall l k. IncorrectInput (Term l k a)


deriving instance Typeable (TypeableError a)
deriving instance (Show a) => Show (TypeableError a)

instance ToJSON a => ToJSON (TypeableError a) where
    toJSON (TypeMismatch a ct1 ct2) = object [
        "tag" .= ("TypeMismatch" :: Text),
        "contents" .= object [
                "term" .= toJSON a,
                "expectedType" .= ct2,
                "foundType" .= ct1
            ]
        ]
    toJSON (LevelMismatch a ct1 ct2) = object [
        "tag" .= ("LevelMismatch" :: Text),
        "contents" .= object [
                "term" .= toJSON a,
                "expectedLevel" .= ct2,
                "foundLevel" .= ct1
            ]
        ]
    toJSON (IncorrectInput t) = object [
        "tag" .= ("IncorrectInput" :: Text),
        "contents" .= toJSON t
        ]

deriving instance (Show a, Typeable a) => Exception (TypeableError a)


connMap :: MonadReader r m => (r -> [ConnDescription t]) -> m (Map Text (ConnDescription t))
connMap f = do
    fConns <- asks f
    return $ M.fromList $ map (\c@ConnDescription{..} -> (name, c)) fConns

connMap' :: forall l r m. (Monad m, SingI l) => CalcMT r m (Map Text (ConnDescription l))
connMap' = connMap'' (fromSing (sing :: Sing l))
    where
        connMap'' AtomL = return M.empty
        connMap'' FormulaL = do
            m <- connMap formulaConns
            return $ unsafeCoerce m
        connMap'' StructureL = do
            m <- connMap structureConns
            return $ unsafeCoerce m


class TypeableCTerm l where
    typeableC' :: (MonadReader (FinTypeCalculusDescription r) m , MonadThrowJSON m, Ord a, Show a, Typeable a, ToJSON a) =>
        Map a CalcType -> CalcType -> Term l 'ConcreteK a -> m (Map a CalcType)



typeableCon :: (MonadReader d m, MonadThrowJSON m) =>
     (map -> CalcType -> trm -> m map) -> (d -> [ConnDescription l]) -> map -> CalcType -> Text -> [trm] -> m map
typeableCon tyF f acc t c xs = do
    conns <- connMap f
    let ConnDescription{..} = conns M.! c
    if t == outType then do
        let tsxs = zip inTypes xs
        foldrM (\(t', x) acc' -> tyF acc' t' x) acc tsxs
    else throw $ TypeMismatch c t outType


instance TypeableCTerm 'AtomL where
    typeableC' acc t (Base a) = if a `M.member` acc then
        let t' = acc M.! a in if t == t' then return $ acc else throw $ TypeMismatch a t t'
        else return $ M.insert a t acc
    typeableC' _ _ trm = throw $ IncorrectInput trm

instance TypeableCTerm 'FormulaL where
    typeableC' acc t (Lift a) = typeableC' acc t a
    typeableC' acc t (Con (C c) vs) = typeableCon typeableC' formulaConns acc t c (toList vs)

instance TypeableCTerm 'StructureL where
    typeableC' acc t (Lift a) = typeableC' acc t a
    typeableC' acc t (Con (C c) vs) = typeableCon typeableC' structureConns acc t c (toList vs)


typeableC :: (MonadReader (FinTypeCalculusDescription r) m , MonadThrowJSON m, Ord a, Show a, Typeable a, ToJSON a, TypeableCTerm l) => CalcType ->
    Term l 'ConcreteK a -> m (Map a CalcType)
typeableC = typeableC' M.empty


typeableCDSeq' :: (MonadReader (FinTypeCalculusDescription r) m , MonadThrowJSON m, Ord a, Show a, Typeable a, ToJSON a) =>
    Map a CalcType -> DSequent 'ConcreteK a -> m (Map a CalcType)
typeableCDSeq' acc (DSeq l t r) = do
    lacc <- typeableC' acc t l
    typeableC' lacc t r




-- typeable MTerm now has two functions. it check that the variables are of the right type, i.e. A doesnt appear as an atprop variable
-- as well as an agent variable. TypeableMTerm also fixes the level of the metavariables, as they are all parsed as AtomL meta-vars
-- due to the ambiguity and the exponential blowup of the possible parse trees. This way, we get a deterministic parse and at type-checking,
-- try to get the most general levels of all MV's. the metavariables can still be manually disambiguate by prepending the name with a_/at_ for an
-- atomL MV, f_ for formulaL and s_ for structureL. if left unspecified, the MV will default to a structureL one, unless it appears under a
-- formula connective, in which case it is lowered to formulaL. we do two passes over each term, first type-checking and figuring out the highest levels
-- of all the MVs and second time (using fixMeta) to raise the MV to the highest possible level, which was determined from the previous pass in the
-- Map Text (Level, CalcType)

class TypeableMTerm l where
    typeableM' :: (MonadReader (FinTypeCalculusDescription r) m , MonadThrowJSON m) => Map Text (Level, CalcType) -> Level -> CalcType -> Term l 'MetaK Text -> m (Map Text (Level, CalcType))
    fixMeta :: (MonadThrowJSON m) => Map Text (Level, t) -> Term l 'MetaK Text -> m (Term l 'MetaK Text)

typeableMeta :: (MonadThrowJSON m) =>
    Map Text (Level, CalcType) -> Level -> CalcType -> Text -> m (Map Text (Level, CalcType))
typeableMeta acc l t v =
    if v `M.member` acc then
        let (_, t') = acc M.! v in case t == t' of
            True  -> return $ insert v t l acc
            False -> throw $ TypeMismatch v t t'
    else return $ insert v t l acc

    where
        insert :: Text -> t -> Level -> Map Text (Level, t) -> Map Text (Level, t)
        insert iv it _ iacc | (T.isPrefixOf "a_" $ T.toLower iv) || (T.isPrefixOf "at_" $ T.toLower iv) =
            M.insert v (AtomL, it) iacc
        insert iv it _ iacc | T.isPrefixOf "f_" $ T.toLower iv = M.insert iv (FormulaL, it) iacc
        insert iv it _ iacc | T.isPrefixOf "s_" $ T.toLower iv = M.insert iv (StructureL, it) iacc
        insert iv it il iacc | iv `M.member` iacc = let (il', _) = acc M.! iv in M.insert iv (min il il', it) iacc
        insert iv it il iacc = M.insert iv (il, it) iacc


typeableMCon :: (MonadReader d m, MonadThrowJSON m) =>
     (map -> Level -> CalcType -> trm -> m map) -> (d -> [ConnDescription l]) -> map -> Level -> CalcType -> Text -> [trm] -> m map
typeableMCon tyF f acc l t c xs = do
    conns <- connMap f
    let ConnDescription{..} = conns M.! c
    if t == outType then do
        let tsxs = zip inTypes xs
        foldrM (\(t', x) acc' -> tyF acc' l t' x) acc tsxs
    else throw $ TypeMismatch c t outType



instance TypeableMTerm 'AtomL where
    typeableM' acc l t (Meta a) = typeableMeta acc l t a
    typeableM' _ _ _ trm        = throw $ IncorrectInput trm

    fixMeta acc (Meta a) | fst (acc M.! a) == AtomL = return $ Meta a
    fixMeta acc (Meta a) | otherwise = throw $ LevelMismatch a AtomL $ fst (acc M.! a)
    fixMeta _ trm = throw $ IncorrectInput trm


instance TypeableMTerm 'FormulaL where
    typeableM' acc l t (Lift a) = typeableM' acc l t a
    typeableM' acc _ t (Con (C c) vs) = typeableMCon typeableM' formulaConns acc FormulaL t c (toList vs)
    typeableM' _ _ _ trm = throw $ IncorrectInput trm

    fixMeta acc (Lift (Meta a)) | fst (acc M.! a) == FormulaL = return $ Meta a
    fixMeta acc (Lift (Meta a)) | fst (acc M.! a) == AtomL = return $ Lift $ Meta a
    fixMeta acc (Lift (Meta a)) | otherwise = throw $ LevelMismatch a FormulaL $ fst (acc M.! a)
    fixMeta acc (Con c vs) = do
        vs' <- mapM (fixMeta acc) vs
        return $ Con c vs'
    fixMeta _ trm = throw $ IncorrectInput trm


instance TypeableMTerm 'StructureL where
    typeableM' acc l t (Lift a) = typeableM' acc l t a
    typeableM' acc _ t (Con (C c) vs) = typeableMCon typeableM' structureConns acc StructureL t c (toList vs)
    typeableM' _ _ _ trm = throw $ IncorrectInput trm

    fixMeta acc (Lift (Lift (Meta a))) | fst (acc M.! a) == StructureL = return $ Meta a
    fixMeta acc (Lift (Lift (Meta a))) | fst (acc M.! a) == FormulaL = return $ Lift $ Meta a
    fixMeta acc (Lift (Lift (Meta a))) | fst (acc M.! a) == AtomL = return $ Lift $ Lift $ Meta a
    fixMeta acc (Lift x) = do
        x' <- fixMeta acc x
        return $ Lift x'
    fixMeta acc (Con c vs) = do
        vs' <- mapM (fixMeta acc) vs
        return $ Con c vs'
    fixMeta _ trm = throw $ IncorrectInput trm


typeableM :: (MonadReader (FinTypeCalculusDescription r) m , MonadThrowJSON m, TypeableMTerm l) => CalcType ->
    Term l 'MetaK Text -> m (Map Text (Level, CalcType))
typeableM = typeableM' M.empty StructureL


typeableMDSeq' :: (MonadReader (FinTypeCalculusDescription r) m , MonadThrowJSON m) =>
    Map Text (Level, CalcType) -> DSequent 'MetaK Text -> m (Map Text (Level, CalcType))
typeableMDSeq' acc (DSeq l t r) = do
    lacc <- typeableM' acc StructureL t l
    typeableM' lacc StructureL t r

fixMDSeq :: (MonadThrowJSON m) => Map Text (Level, t) -> DSequent 'MetaK Text -> m (DSequent 'MetaK Text)
fixMDSeq acc (DSeq l t r) = do
    l' <- fixMeta acc l
    r' <- fixMeta acc r
    return $ DSeq l' t r'


typeableRule :: (MonadReader (FinTypeCalculusDescription r) m , MonadThrowJSON m) => Rule Text -> m (Map Text (Level, CalcType))
typeableRule (Rule _ premises conclusion) = do
    conclAcc <- typeableMDSeq' M.empty conclusion
    foldrM (\dseq acc -> typeableMDSeq' acc dseq) conclAcc premises
typeableRule (RevRule _ premise conclusion) = do
    conclAcc <- typeableMDSeq' M.empty conclusion
    typeableMDSeq' conclAcc premise


fixRule :: (MonadThrowJSON m) => Map Text (Level, t) -> Rule Text -> m (Rule Text)
fixRule acc (Rule name premises conclusion) = do
    concl' <- fixMDSeq acc conclusion
    prems' <- mapM (fixMDSeq acc) premises
    return $ Rule name prems' concl'
fixRule acc (RevRule name premise conclusion) = do
    concl' <- fixMDSeq acc conclusion
    prem' <- fixMDSeq acc premise
    return $ RevRule name prem' concl'
