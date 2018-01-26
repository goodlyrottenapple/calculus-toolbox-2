{-|
Module      : Lib
Description : Lib's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}

{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}

module Rules where

import           Lib.Prelude
import           Terms

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Prelude             as P


data PT a = PT {
    premises   :: [PT a]
  , name       :: Text
  , conclusion :: a
} deriving (Show, Functor)

instance ToJSON a => ToJSON (PT a) where
    toJSON (PT xs r c) = object [
        r .= object [
                "premises" .= toJSON xs,
                "conclusion" .= toJSON c
            ]
        ]

instance FromJSON a => FromJSON (PT a) where
    parseJSON = withObject "proof tree" $ \o ->
        case  HM.keys o of
            [r] -> do
                pt <- (.:) @(HM.HashMap Text Value) o r
                ps <- pt .: "premises"
                c <- pt .: "conclusion"
                return $ PT ps r c
            _ -> P.fail "Invalid proof tree"



unify :: (Ord a, Ord b) => MetaTerm l a -> ConcreteTerm l b -> Maybe (Map a (ConcreteTerm l b))
unify (Meta x) t@(Abbrev _ _) = Just $ M.singleton x t
unify m@(Lift _) (Abbrev _ c@(Lift _)) = unify m c
unify m@(Lift _) (Abbrev _ (Abbrev _ c)) = unify m c
unify m@(Con _ _) (Abbrev _ c@(Con _ _)) = unify m c
unify m@(Con _ _) (Abbrev _ (Abbrev _ c)) = unify m c

unify (Meta x) c = Just $ M.singleton x c
unify (Lift lx) (Lift ly) = case unify lx ly of
    Just u  ->  Just $ M.map (\t -> Lift t) u
    Nothing -> Nothing

unify (Con (C c1) vs) (Con (C c2) us) = case eqLen vs us of
    Just Refl -> if c1 == c2 then unifyC vs us else Nothing
    Nothing   -> Nothing

unify _ _ = Nothing


unifyC :: (Ord a, Ord b) =>
    Vec n (MetaTerm l a) -> Vec n (ConcreteTerm l b) -> Maybe (Map a (ConcreteTerm l b))
unifyC xs ys = foldr unifyFold (Just $ M.empty) (zipV xs ys)
    where
        unifyFold _ Nothing = Nothing
        unifyFold (x,y) (Just ws) = do
            u <- unify x y
            let uInteresction = M.intersection u ws
                c = M.foldrWithKey (\k v b -> checkConsistent k v ws && b) True uInteresction
            if c then return $ M.union u ws else Nothing

        checkConsistent :: (Ord k, Eq v) => k -> v -> Map k v -> Bool
        checkConsistent k v1 m | (Just v2) <- M.lookup k m = v1 == v2
                               | otherwise = False


unifySeq :: (Ord a, Ord b) => DSequent 'MetaK a -> DSequent 'ConcreteK b -> Maybe (Map a (ConcreteTerm 'StructureL b))
unifySeq (DSeq ml t1 mr) (DSeq cl t2 cr) | t1 == t2 = unifyC (ml :> mr :> Nil) (cl :> cr :> Nil)
                                         | otherwise = Nothing

sub :: Ord a => Map a (ConcreteTerm l b) -> MetaTerm l a -> Maybe (ConcreteTerm l b)
sub m (Meta x) = M.lookup x m
sub m (Lift lx) = do
    let m' = M.foldrWithKey pickLifted M.empty m
    t <- sub m' lx
    return $ Lift t
    where
        pickLifted k (Lift x) m' = M.insert k x m'
        pickLifted _ _ m'        = m'
sub m (Con (C c) xs) = do
    xs' <- traverse (sub m) xs
    return $ Con (C c) xs'


subSeq :: Ord a => Map a (ConcreteTerm 'StructureL b) -> DSequent 'MetaK a -> Maybe (DSequent 'ConcreteK b)
subSeq m (DSeq l n r) = do
    lsub <- sub m l
    rsub <- sub m r
    return $ DSeq lsub n rsub



type RuleName = Text


isApplicable :: (Ord a, Ord b) => Rule a -> DSequent 'ConcreteK b -> Maybe (Either [DSequent 'ConcreteK b] [DSequent 'ConcreteK b])
isApplicable (Rule _ _ premises conclusion) dseq = do
    udict <- unifySeq conclusion dseq
    liftM Left $ mapM (subSeq udict) premises
isApplicable (RevRule _ _ premise conclusion) dseq = r1 <|> r2 -- we assume that a reversible rule should not be applicable in both direcions!!
    where
        r1 = do
            udict <- unifySeq conclusion dseq
            liftM (Left . (:[])) $ subSeq udict premise
        r2 = do
            udict <- unifySeq premise dseq
            liftM (Left . (:[])) $ subSeq udict conclusion

getApplicableRules :: (MonadReader (FinTypeCalculusDescription [Rule Text]) m, MonadThrowJSON m, Ord b) => DSequent 'ConcreteK b ->
    m (Map RuleName [DSequent 'ConcreteK b])
getApplicableRules dseq = do
    rs <- asks rules
    return $ foldr (\r m -> case isApplicable r dseq of {
        (Just (Left ps))  -> M.insert (ruleName r) ps m;
        (Just (Right ps)) -> M.insert (ruleName r <> "Rev") ps m;
        Nothing   -> m}) M.empty rs


cutRule :: CalcType -> Rule Text
cutRule typ@(Type t) = Rule{..}
    where
        name = "Cut" <> t
        latexSyntax = Just $ "Cut_{" <> t <> "}"
        prems = [DSeq (Meta "X") typ (Lift (Meta "A")), DSeq (Lift (Meta "A")) typ (Meta "Y")]
        concl = DSeq (Meta "X") typ (Meta "Y")


findProof' :: (MonadReader (FinTypeCalculusDescription [Rule Text]) m, MonadThrowJSON m, Ord b) =>
    Int -> Set (DSequent 'ConcreteK b) -> DSequent 'ConcreteK b -> m [PT (DSequent 'ConcreteK b)]
findProof' 0 _ _ = return []
findProof' n prems s = if s `S.member` prems then return [PT [] "Axiom" s] else do
    appM <- getApplicableRules s
    aux1 s $ M.toList appM

    where
        -- aux1 :: (MonadReader (FinTypeCalculusDescription [Rule Text]) m, MonadThrowJSON m, Ord b) =>
        --     DSequent 'ConcreteK b -> [(RuleName, [DSequent 'ConcreteK b])] -> m [PT (DSequent 'ConcreteK b)]
        aux1 _ [] = return []
        aux1 c ((nm,ps):xs) = do
            x <- aux c nm ps
            xs' <- aux1 c xs
            return $ x ++ xs'

        -- aux :: (MonadReader (FinTypeCalculusDescription [Rule Text]) m, MonadThrowJSON m, Ord b) =>
        --     DSequent 'ConcreteK b -> RuleName -> [DSequent 'ConcreteK b] -> m [PT (DSequent 'ConcreteK b)]
        aux c nm [] = return $ [PT [] nm c]
        aux c nm (p:ps) = do
            pPts <- findProof' (n-1) prems p
            psPts <- aux c nm ps
            return $ concat $ map (\p' -> map (\(PT ps' nm' c') -> PT (p':ps') nm' c') psPts) pPts


findProof :: (MonadReader (FinTypeCalculusDescription [Rule Text]) m, MonadThrowJSON m, Ord b) =>
    Int -> Int -> Set (DSequent 'ConcreteK b) -> DSequent 'ConcreteK b -> m [PT (DSequent 'ConcreteK b)]
findProof n maxIters prems s | n == maxIters = findProof' n prems s
                  | otherwise = do
                    r <- findProof' n prems s
                    case r of
                        [] -> findProof (n+1) maxIters prems s
                        _  -> return r



