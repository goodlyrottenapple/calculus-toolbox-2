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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}


module CTTerms.Parser.CTTermParser(tokenizeCTTerm, gConcreteCTTerm, testC, testM) where

import CTTerms.Core
import CTTerms.Parser.Core

import CTTerms.Typing.DescParser
import CTTerms.Parser.DescParser

import           Lib.Prelude hiding (Associativity, Infix, Prefix, Fixity, Type, Any)
import qualified Prelude            as P

import           Data.Aeson
import           Data.Char
import           Data.HashSet       (HashSet)
import qualified Data.HashSet       as HS
import qualified Data.Map           as M
-- import           Data.Set           (Set)
import qualified Data.Set           as S
import qualified Data.Text          as T
import           Text.Earley
import           Text.Earley.Mixfix(Associativity(..), mixfixExpressionSeparate, Holey)
import Data.Singletons(SingI, sing)


gConcreteCTTerm :: forall l a b. SingI l => (Text -> [Token Text]) -> LevelList l [ConnDescription (Token Text) (Type a b)] ->
    G (CTTerm l 'Concrete () (Token Text))
gConcreteCTTerm tokenizer lst = mdo
    lift <- case sing :: Sing l of
        SFormula -> do 
            r <- gConcreteCTTerm tokenizer (tailLL lst)
            rule $ Lift <$> r
        SStructure -> do 
            r <- gConcreteCTTerm tokenizer (tailLL lst)
            rule $ Lift <$> r
        _ -> rule $ Nm <$> satisfyT (const False) -- this should never be matched!!
    atom  <- rule $ 
                lift
            <|> (namedToken "(" *> expr <* namedToken ")")
    expr  <- mixfixExpressionSeparate table atom
    return expr
    where
        table = mkTableBinding $
            [ (hprod (unTok name) inTypes as,
               associativity fixity, 
               binding fixity, 
               \i xs -> Con (unTok name) (assembleCTTerm i xs) ()) | ConnDescription{..} <- headLL lst ] ++
            [ (hprodLisp (unTok name) inTypes as,
               NonAssoc, 
               maxBound,
               \i xs -> Con (unTok name) (assembleCTTerm i xs) ()) | ConnDescription{..} <- headLL lst ]

        hprod :: Text -> [Type a b] -> Maybe Text -> 
            Holey (Prod r (Token Text) (Token Text) (Either (Token Text) (Token Text)))
        hprod n t as = mkHoleProd reservedCTTerm (mkHole as (toS n) t)

        hprodLisp :: Text -> [Type a b] -> Maybe Text -> 
            Holey (Prod r (Token Text) (Token Text) (Either (Token Text) (Token Text)))
        hprodLisp n ts as = mkHoleProdLisp reservedCTTerm as (tokenizer n) ts
        reservedCTTerm = HS.fromList [ unTok name | ConnDescription{..} <- headLL lst ]


mkHoleProd :: HashSet Text -> [Hole (Token Text)] -> Holey (Prod r (Token Text) (Token Text) (Either (Token Text) (Token Text)))
mkHoleProd _        [] = []
mkHoleProd reserved (Hole:xs) = Nothing : mkHoleProd reserved xs
mkHoleProd reserved (NameHole:xs) = Just prod : mkHoleProd reserved xs
    where
        prod = Left <$> satisfyT (not . (`HS.member` reserved))
mkHoleProd reserved (Tok t:xs) = Just prod : mkHoleProd reserved xs
    where
        prod = Right <$> namedToken t




assembleCTTerm :: Holey (Either a (Token Text)) -> [CTTerm l k t a] -> [CTTerm l k t a]
assembleCTTerm [] _ = []
assembleCTTerm (Nothing:xs) (t:ts) = t:assembleCTTerm xs ts
assembleCTTerm (Just (Left nm):xs) ts = Nm nm:assembleCTTerm xs ts
assembleCTTerm (Just (Right _):xs) ts = assembleCTTerm xs ts
assembleCTTerm _ ts = ts -- this should be unreachable



gMetaCTTerm :: forall l a b. SingI l => (Text -> [Token Text]) -> LevelList l [ConnDescription (Token Text) (Type a b)] ->
    G (CTTerm l 'Meta () (Token Text))
gMetaCTTerm tokenizer lst = mdo
    lift <- case sing :: Sing l of
        SFormula -> do 
            r <- gMetaCTTerm tokenizer (tailLL lst)
            rule $ Lift <$> r
        SStructure -> do 
            r <- gMetaCTTerm tokenizer (tailLL lst)
            rule $ Lift <$> r
        _ -> rule $ Nm <$> satisfyT (not . (`HS.member` reservedCTTerm))
    atom  <- rule $ 
                lift
            <|> (namedToken "(" *> expr <* namedToken ")")
    expr  <- mixfixExpressionSeparate table atom
    return expr
    where
        table = mkTableBinding $
            [ ((map (\x -> Just $ namedToken x) $ tokenizer $ unTok name) ++ map (const Nothing) inTypes, 
               associativity fixity, 
               binding fixity, 
               \_ xs -> Con (unTok name) xs ()) | ConnDescription{..} <- headLL lst ] ++

            [ (map (map namedToken) $ holey as $ toS $ unTok name, 
               associativity fixity, 
               binding fixity, 
               \_ xs -> Con (unTok name) xs ()) | ConnDescription{..} <- headLL lst, name /= "_" ]

        reservedCTTerm = HS.fromList [ unTok name | ConnDescription{..} <- headLL lst ]


tokenizerSettingsCTTerm :: LevelList l [ConnDescription (Token Text) (Type a b)] -> 
    TokenizerSettings
tokenizerSettingsCTTerm llist = defaultTokenizerSettings {
        reserved = HS.toList $ HS.fromList res
      -- , comment = [("--", "\n")]
      -- , block = [("\"", "\""), ("{-#" , "#-}")]
      , special = HS.toList $ HS.fromList $ " \n(){}[],." ++ sp
    }

    where
        (res, sp) = foldr (\(a,b) (c,d) -> (a++c, b++d)) ([],[]) (map getReserved llist)
        getReserved :: [ConnDescription (Token Text) (Type a b)] -> ([P.String], P.String)
        getReserved [] = ([],[])
        getReserved (ConnDescription n _ _ _ Nothing Infix{} _ _:xs) = 
            -- we know that the conn is infix, so it begins and ends with _
            -- and we add it to reserved, if "as" is not set, i.e. we dont have a prefix
            let symb = T.tail $ T.init $ unTok n
                char = T.head symb in
                -- make sure we only add reserved strings which do not begin with a
                -- letter or a number
                if not (isDigit char || isLetter char)
                    then bimap (toS symb:) (char:) $ getReserved xs
                    else getReserved xs
        getReserved (_:xs) = getReserved xs


tokenizeCTTerm :: LevelList l [ConnDescription (Token Text) (Type a b)] ->
    P.String -> [Token Text]
tokenizeCTTerm llist = flip evalState (1,1) . tokenize (tokenizerSettingsCTTerm llist)

testC :: Text -> IO ()
testC str = do
    conns <- test

    putStrLn $ ("tokenizer settings:\n" :: Text) <> show (tokenizerSettingsCTTerm conns)

    let (ps, r) = parseG'' (tokenizeCTTerm conns . toS) (gConcreteCTTerm (tokenizeCTTerm conns . toS) conns) str
    putStrLn $ ("report:\n" :: Text) <> show r
    putStrLn $ ("parses:\n" :: Text) <> (T.intercalate "\n" $ map (show . (bimap identity unTok)) ps)
    putStrLn $ ("unique parses:\n" :: Text) <> (T.intercalate "\n" $ map (show . (bimap identity unTok)) $ S.toList $ S.fromList ps)

testM :: Text -> IO ()
testM str = do
    conns <- test

    putStrLn $ ("tokenizer settings:\n" :: Text) <> show (tokenizerSettingsCTTerm conns)

    let (ps, r) = parseG'' (tokenizeCTTerm conns . toS) (gMetaCTTerm (tokenizeCTTerm conns . toS) conns) str
    putStrLn $ ("report:\n" :: Text) <> show r
    putStrLn $ ("parses:\n" :: Text) <> (T.intercalate "\n" $ map (show . (bimap identity unTok)) ps)
    putStrLn $ ("unique parses:\n" :: Text) <> (T.intercalate "\n" $ map (show . (bimap identity unTok)) $ S.toList $ S.fromList ps)
