{-# LANGUAGE TemplateHaskell #-}

module Terms.TH where

import Lib.Prelude
import qualified Prelude as P
import Language.Haskell.TH

-- generateMkHomCon :: Text -> DecsQ
-- generateMkHomCon v = do
--    (ClassI (ClassD _ cls _ _ _) _) <- reify $ mkName "MkHomCon"
--    (FamilyI _ instances) <- reify $ mkName "HomCon"
--    (Just homC) <- lookupValueName "HomC"
--    let ins' = filter (\x -> getVersion x == (toS v)) instances

--    case ins' of
--         [i] -> do
--             let cons' = getConstructors i
--                 cons = groupConstructors cons'
--             mapM (\cs@(c:_) -> mkInstance cls homC (getLevel c) cs ) cons
--    where

--         groupConstructors [] = [] 
--         groupConstructors (x:xs) = (x:(filter (\y -> getLevel x == getLevel y) xs)) : groupConstructors (filter (\y -> getLevel x /= getLevel y) xs)

--         getVersion (DataInstD _ _ ((LitT (StrTyLit v)):_) _ _ _) = v
--         getConstructors (DataInstD _ _ _ _ cs _) = cs

--         getName :: Con -> Name
--         getName (GadtC [ n ] _ _) = n

--         appTtoList (AppT x y) = (appTtoList x) ++ (appTtoList y)
--         appTtoList x = [x]

--         getLevel ::  Con -> Name
--         getLevel (GadtC _ _ ty) = case (appTtoList ty) ! 2 of
--             (Just (ConT l)) -> l

--         getParserSymbol ::  Con -> P.String
--         getParserSymbol (GadtC _ _ ty) = case (appTtoList ty) ! 5 of
--             (Just (LitT (StrTyLit s))) -> s

--         mkInstance cls homC level is = instanceD 
--             (return [])
--             (appT (appT (conT cls) (litT (strTyLit $ toS v))) (promotedT $ level))
--             [(funD (mkName "mkHomCon") (
--                 (map (mkClause homC) $ is) ++ [ defC ]
--             ))]

--         defC = clause [wildP,wildP] (normalB $ conE 'Nothing) []
--         mkClause homC c = clause [litP $ stringL $ getParserSymbol c , varP $ mkName "xs"] (normalB [| do { v <- toVec Proxy xs ; Just $ $(conE $ homC) $(conE $ getName c) v } |]) []

-- generateParserData :: Text -> DecsQ
-- generateParserData v = map (:[]) $ do
--    (ClassI (ClassD _ cls _ _ _) _) <- reify $ mkName "ParserData"
--    (FamilyI _ instances) <- reify $ mkName "HomCon"
--    let ins' = filter (\x -> getVersion x == (toS v)) instances

--    case ins' of
--         [i] -> do
--             let cons = getConstructors i
--             mkInstance cls cons
--    where

--         getVersion (DataInstD _ _ ((LitT (StrTyLit v)):_) _ _ _) = v
--         getConstructors (DataInstD _ _ _ _ cs _) = cs

--         getName :: Con -> Name
--         getName (GadtC [ n ] _ _) = n


--         mkInstance cls is = instanceD 
--             (return [])
--             (appT (conT cls) (litT (strTyLit $ toS v)))
--             [(funD (mkName "getParserData") [ defC (varE $ mkName "mkParserDataC") is ])]

--         defC f is = clause [wildP] (normalB $ foldr (\i xpr -> infixE (Just $ appE f (conE $ getName i)) (conE '(:)) (Just xpr)) (conE '[]) is) []

--         -- partition 