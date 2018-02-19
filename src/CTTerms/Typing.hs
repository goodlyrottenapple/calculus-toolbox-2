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
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}


module CTTerms.Typing where

import CTTerms.Core
import CTTerms.Parser

import           Lib.Prelude
import qualified Prelude            as P
-- import           Data.HashSet       (HashSet)
import qualified Data.HashSet       as HS
import qualified Data.Map           as M
import qualified Data.Text          as T

mkFreshVar :: Int -> Text
mkFreshVar fresh = "t" <> show fresh


-- a special kind of map lookup, where, if a map doesnt contain the given key, the key is returned
-- we can use this to handle constants vs variables.... i.e. if we write {Fm, Ag, Act, Fnc} and 
-- Fm, Ag, ... etc are not in the map, they are treated as constants...

(?!) :: Ord k => Map k k -> k -> k
m ?! k = case M.lookup k m of
    Just v -> v
    Nothing -> k


type ConMap = Map Text ([CTTerms.Core.Type (Text, [FormulaLang Text Text]) Text], (CTTerms.Core.Type (Text, [FormulaLang Text Text]) Text))
type SetNameMap = Map Text Text
type ElemNameMap = Map Text Text



fixNm :: CTTerm l k t a -> CTTerm l k t a
fixNm (Lift (Nm a)) = Nm a
fixNm (Lift (Lift (Nm a))) = Nm a
fixNm (Lift x) = Lift (fixNm x)
fixNm (Con c xs t) = Con c (map fixNm xs) t
fixNm x = x



data ApplyTypeState = ApplyTypeState {
    setNameMap :: SetNameMap,
    elemNameMap :: ElemNameMap,
    fresh :: Int
}



replaceAssert' :: MonadState ApplyTypeState m => FormulaLang Text Text -> m (FormulaLang Text Text)
replaceAssert' a = do
    setNmMap <- gets setNameMap
    elemNmMap <- gets elemNameMap
    return $ bimap (elemNmMap ?!) (setNmMap M.!) a



applyTypes' :: (MonadState ApplyTypeState m, MonadError Text m) => 
    ConMap -> CTTerm l 'Concrete (Maybe Text, [FormulaLang Text Text]) Text ->
    m (CTTerm l 'Concrete (Maybe Text, [FormulaLang Text Text]) Text)
applyTypes' conMap (Lift x) = do
    liftM Lift $ applyTypes' conMap x
applyTypes' conMap (Con n args (Nothing, []))
    | n `M.member` conMap = do
        args' <- applyTypesList' conMap args (fst $ conMap M.! n)
        -- _ <- applyTypesList' conMap args (fst $ conMap M.! n) `debug` ("\n\ncon: " ++ (toS n) ++ "\nret: " ++ show args')

        f <- gets fresh
        modify (\s -> s{fresh = f+1})
        let fVar = mkFreshVar f
        case snd $ conMap M.! n of
            (CType _ Nothing) -> return $ Con n args' (Just fVar, [])
            (CType _ (Just (v, cs))) -> do
                modify (\s@ApplyTypeState{..} -> s{setNameMap = M.insert v fVar setNameMap})
                cs' <- mapM replaceAssert' cs
                return $ Con n args' (Just fVar, cs')
            (NVar _) -> throwError "cannot be of type NVar"
            (CListType _ _) -> throwError "CListType not implmeented yet"
    | otherwise = throwError "the connective wasn't declared"
applyTypes' conMap (Con n args ((Just fVar), cs))
    | n `M.member` conMap = do
        args' <- applyTypesList' conMap args (fst $ conMap M.! n)
        case snd $ conMap M.! n of
            (CType _ Nothing) -> return $ Con n args (Just fVar, cs)
            (CType _ (Just (v, cs'))) -> do
                modify (\s@ApplyTypeState{..} -> s{setNameMap = M.insert v fVar setNameMap})
                cs'' <- mapM replaceAssert' cs'
                return $ Con n args (Just fVar, cs'' ++ cs)
            (NVar _) -> throwError "cannot be of type NVar"
            (CListType _ _) -> throwError "CListType not implmeented yet"
    | otherwise = throwError "the connective wasn't declared"



applyTypesList' :: (MonadState ApplyTypeState m, MonadError Text m) => 
    ConMap ->
    [CTTerm l 'Concrete (Maybe Text, [FormulaLang Text Text]) Text] ->
    [CTTerms.Core.Type (Text, [FormulaLang Text Text]) Text] ->
    m [CTTerm l 'Concrete (Maybe Text, [FormulaLang Text Text]) Text]
applyTypesList' _      [] [] = return []
applyTypesList' conMap (x@(Nm _):xs) (NVar Nothing:ys) = do
    xs' <- applyTypesList' conMap xs ys
    return $ x:xs'
applyTypesList' conMap (x@(Nm n):xs) (NVar (Just nm):ys) = do
    modify (\s@ApplyTypeState{..} -> s{elemNameMap = M.insert nm n elemNameMap})
    xs' <- applyTypesList' conMap xs ys
    return $ x:xs'
applyTypesList' conMap ((Lift x):xs) (y:ys) = do
    x' <- liftM Lift $ applyTypes' conMap x
    xs' <- applyTypesList' conMap xs ys
    return $ x':xs'
applyTypesList' conMap (con@(Con _ _ _):xs) (CType _ Nothing:ys) = do
    con' <- applyTypes' conMap con
    xs' <- applyTypesList' conMap xs ys
    return $ con':xs'
applyTypesList' conMap (con@(Con _ _ _):xs) (CType _ (Just (v, cs)):ys) = do
    Con c args (Just fVar , cs') <- applyTypes' conMap con 
    cs'' <- do
        modify (\s@ApplyTypeState{..} -> s{setNameMap = M.insert v fVar setNameMap}) 
        mapM replaceAssert' cs
    xs' <- applyTypesList' conMap xs ys 
    return $ Con c args (Just fVar , cs'' ++ cs'):xs' 


-- collectAsserts :: CTTerm l k (Maybe Text, [FormulaLang Text Text]) Text -> [FormulaLang Text Text]
-- collectAsserts (Lift x) = collectAsserts x
-- collectAsserts (Con _ xs (Nothing,_)) = concat $ map collectAsserts xs
-- collectAsserts (Con _ xs (Just s,cs)) = SetDecl s:cs ++ (concat $ map collectAsserts xs)
-- collectAsserts _ = []

type M s e a = StateT s (Except e) a


runM :: s -> M s e a -> Either e a
runM s sea = runExcept $ evalStateT sea s 

-- test :: IO ()
-- test = do
--     let
--         rT = HS.fromList["var", "pred", "fun"]
--         rF = HS.fromList["all", "dot"]
--         rS = HS.empty
--         parseT = first fixNm . parseG (gCTTermLisp @'Term    @'Concrete rT rF rS) 
--         parseF = first fixNm . parseG (gCTTermLisp @'Formula @'Concrete rT rF rS)
--         (Left var_def) = parseG gDecl "var :: (z : Name) -> Term (t where t = {z})"
--         (Left var2_def) = parseG gDecl "var2 :: Name -> Term (t where t < {Ag, Act, Fm, Fnc} ; card t = 1)"
--         (Left pred_def) = parseG gDecl "pred :: Name -> Term t -> Term (t' where t' = t)"
--         (Left and_def) = parseG gDecl "and :: Formula t -> Formula t' -> Formula (t'' where t'' = t ; t'' = t')"
--         (Left all_def) = parseG gDecl "all :: (x : Name) -> Formula (t where x : t) -> Formula (t' where t' = t - {x})"
--         (Left dot_def) = parseG gDecl "dot :: (y : Name) -> Formula (t where t & {y} = {}) -> Formula (t' where t' = t | {y})"
--         (Left varx) = parseT "(var g)"
--         -- (Left predx) = parseT "(pred P (var zaaaa))"
--         (Left allx) = parseF "(all x (pred P (var x)))"
--         (Left allxy) = parseF "(all y (all x (dot y (pred P (var x)))))"
--         conMap = M.fromList $ map (\(Decl n xs y) -> (n , (xs, y))) [var_def, pred_def, all_def, dot_def]
--         s = ApplyTypeState M.empty M.empty 0

--         (Right allx_asserts) = second collectAsserts $ runM s $ applyTypes' conMap allx
--         (Right allxy_asserts) = second collectAsserts $ runM s $ applyTypes' conMap allxy
--     -- print and_def
--     -- print pred_def
--     -- print all_def
--     -- print varx
--     -- print predx
--     print allx
--     -- print $ runM s $ applyTypes' conMapT varx
--     -- print $ runM s $ applyTypes' conMap predx

--     print $ runM s $ applyTypes' conMap allx
--     putStrLn $ T.intercalate "\n" $ map printAssert allx_asserts

--     -- print allxy
--     -- putStrLn $ T.intercalate "\n" $ map printAssert allxy_asserts

--     -- print $ applyTypes conMapT M.empty M.empty 0 predx
