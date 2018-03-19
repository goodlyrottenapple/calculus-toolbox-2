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


module CTTerms.Parser.CTTermParser(tokenizeCTTerm, gCTTerm, testC, testM, stripTok) where

import CTTerms.Core
import CTTerms.Parser.Core

import CTTerms.Typing.DescParser
-- import CTTerms.Parser.DescParser

import           Lib.Prelude hiding (Associativity, Infix, Prefix, Fixity, Type, Any, Meta)
import qualified Prelude            as P

-- import           Data.Aeson
import           Data.Char
import           Data.HashSet       (HashSet)
import qualified Data.HashSet       as HS
-- import qualified Data.Map           as M
-- import           Data.Set           (Set)
import qualified Data.Set           as S
import qualified Data.Text          as T
import           Text.Earley
import           Text.Earley.Mixfix(Associativity(..), mixfixExpressionSeparate, Holey)
import Data.Singletons(SingI, sing, fromSing)


gCTTerm :: forall k l a b. (SingI l, SingI k) => (Text -> [Token Text]) -> LevelList l [ConnDescription (Token Text) (Type a b)] ->
    G (CTTerm l k () (Token Text))
gCTTerm tokenizer lst = mdo
    lift <- case (sing :: Sing l, sing :: Sing k) of
        (STerm, SMeta) -> rule $ Nm <$> satisfyT (not . (`HS.member` reservedCTTerm))
        (SFormula, _) -> do 
            r <- gCTTerm tokenizer (tailLL lst)
            rule $ Lift <$> r
        (SStructure, _) -> do 
            r <- gCTTerm tokenizer (tailLL lst)
            rule $ Lift <$> r
        _ -> rule $ Lib.Prelude.empty
    atom  <- rule $ 
                lift
            <|> (namedToken "(" *> expr <* namedToken ")")
    expr  <- case sing :: Sing l of
        -- custom parsing for sequents since we only want Con _ _ _ to be parsed, not Lift _
        SSequent -> do
            str <- gCTTerm tokenizer (tailLL lst)
            rule $ foldr (<|>) (namedToken "(" *> expr <* namedToken ")") $ (seqParserAux tokenizer lst str)
        _ -> mixfixExpressionSeparate table atom
    return expr
    where
        table = mkTableBinding $ 
            -- production for normal mixfix parsing
            [   (hprod (unTok name) inTypes as,
                 associativity fixity, 
                 binding fixity, 
                 \i xs -> Con (unTok name) (assembleCTTerm i xs) ()) | ConnDescription{..} <- headLL lst , 
                -- if we are parsing a meta term, remove the implicit _ connective to avoid ambiguity between Con "_" (Nm x) and Nm x when parsing `x`
                -- we can still explicitly parse the former by writing `_ x`
                (fromSing (sing :: Sing k) == Meta) ~> (name /= "_") ] ++
            -- production for lisp style, perfix parsing, e.g. _/\_ a b
            [   (hprodLisp (unTok name) inTypes as,
                 NonAssoc, 
                 maxBound,
                 \i xs -> Con (unTok name) (assembleCTTerm i xs) ()) | ConnDescription{..} <- headLL lst ] ++
            -- production for a formula level abbreviation, which has the type ({{_}}) : Name -> Formula (abbrev)
            case (sing :: Sing l, sing :: Sing k) of
                (SFormula, SConcrete) -> 
                    [   (mkHoleProd reservedCTTerm [Tok "{{", NameHole, Tok "}}"],
                         NonAssoc, 
                         maxBound, 
                         \[_,Just(Left nm),_] _ -> Abbrev (unTok nm) Nothing) ]
                _ -> []

        hprod :: Text -> [Type a b] -> Maybe Text -> 
            Holey (Prod r (Token Text) (Token Text) (Either (Token Text) (Token Text)))
        hprod n t as = mkHoleProd reservedCTTerm (mkHole as (toS n) t)

        hprodLisp :: Text -> [Type a b] -> Maybe Text -> 
            Holey (Prod r (Token Text) (Token Text) (Either (Token Text) (Token Text)))
        hprodLisp n ts as = mkHoleProdLisp reservedCTTerm as (tokenizer n) ts

        reservedCTTerm = HS.fromList [ unTok name | ConnDescription{..} <- headLL lst ]


-- mkHole prod creates a Holey Either Tok Tok production rule from a Hole, sending
--    Hole -> Nothing
--    NameHole -> Left <name parser>
--    Tok -> Right <name token>
-- this allows us to parse names in defns. like Name -> Formula -> Formula in a proper way
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


-- a helper production rule for parsing sequents.
seqParserAux :: (Text -> [Token Text]) -> LevelList 'Sequent [ConnDescription (Token Text) (Type a b)] ->
    Prod r (Token Text) (Token Text) (CTTerm 'Structure k () (Token Text)) -> 
    [Prod r (Token Text) (Token Text) (CTTerm 'Sequent k () (Token Text))]
seqParserAux tokenizer lst strProd = 
    -- production for normal mixfix parsing
    [ (\xs -> Con (unTok name) (map Lift xs) ()) <$> 
        (args strProd $ holey as $ toS $ unTok name) | ConnDescription{..} <- headLL lst ] ++
    -- production for lisp style, perfix parsing, e.g. _|-_ a b
    [ (\xs -> Con (unTok name) (map Lift xs) ()) <$> 
        (argsLisp strProd (tokenizer $ unTok name) (length inTypes)) | ConnDescription{..} <- headLL lst ]
    where
        args :: Prod r (Token Text) (Token Text) (CTTerm 'Structure k () (Token Text)) -> Holey (Token Text) ->
            Prod r (Token Text) (Token Text) [CTTerm 'Structure k () (Token Text)]
        args _ [] = Lib.Prelude.empty
        args str [Nothing] = (:[]) <$> str
        args _ [Just tok] = (const []) <$> namedToken tok
        args str (Nothing:xs) = (:) <$> str <*> args str xs
        args str (Just tok:xs) = namedToken tok *> args str xs

        argsLisp :: Prod r (Token Text) (Token Text) (CTTerm 'Structure k () (Token Text)) -> [Token Text] -> Int ->
            Prod r (Token Text) (Token Text) [CTTerm 'Structure k () (Token Text)]
        argsLisp str con noOfArgs = foldr (*>) (args str (replicate noOfArgs Nothing)) (map namedToken con)


tokenizerSettingsCTTerm :: LevelList l [ConnDescription (Token Text) (Type a b)] -> 
    TokenizerSettings
tokenizerSettingsCTTerm llist = defaultTokenizerSettings {
        reserved = HS.toList $ HS.fromList (res ++ ["{{", "}}"])
      , special = HS.toList $ HS.fromList $ " \n(){}[],." ++ sp
    }

    where
        (res, sp) = foldr (\(a,b) (c,d) -> (a++c, b++d)) ([],[]) (map getReserved llist)
        getReserved :: [ConnDescription (Token Text) (Type a b)] -> ([P.String], P.String)
        getReserved [] = ([],[])
        getReserved (ConnDescription n _ _ _ _ Mixfix{} _ _:xs) = 
            -- we know that the conn is infix, so it begins and ends with _
            let symbs = filter (not . T.null) $ map T.strip $ T.splitOn "_" $ unTok n
                -- make sure we only add reserved strings which do not begin with a
                -- letter or a number
                -- we only add to reserved if the lenght of the string is > 1
                adding = map toS $ filter 
                    (\symb -> T.length symb > 1 && 
                        not (isDigit (T.head symb) || isLetter (T.head symb))) symbs
                addingChar = map T.head $ filter 
                    (\symb -> T.length symb >= 1 && 
                        not (isDigit (T.head symb) || isLetter (T.head symb))) symbs in

                bimap (adding ++) (addingChar ++) $ getReserved xs
        getReserved (ConnDescription n _ _ _ _ f _ _:xs) = 
            -- we know that if the conn is prefix, we strip _ off the end
            -- if it is an infix, we strip _ from the beginning and the end
            let symb = case f of {Infix{} -> T.tail $ T.init $ unTok n; _ -> T.init $ unTok n}
                char = T.head symb in
                -- make sure we only add reserved strings which do not begin with a
                -- letter or a number
                if T.length symb > 1 && not (isDigit char || isLetter char) 
                    then bimap (toS symb:) (char:) $ getReserved xs
                    else if T.length symb == 1 && not (isDigit char || isLetter char)
                        then bimap identity (char:) $ getReserved xs
                        else getReserved xs


tokenizeCTTerm :: LevelList l [ConnDescription (Token Text) (Type a b)] ->
    P.String -> [Token Text]
tokenizeCTTerm llist = flip evalState (1,1) . tokenize (tokenizerSettingsCTTerm llist)



stripTok :: LevelList l [ConnDescription (Token c) (Type (Token a) (Token b))] -> LevelList l [ConnDescription c (Type a b)]
stripTok = map (map (bimap unTok (bimap unTok unTok)))

testC :: Text -> IO ()
testC str = do
    conns <- test

    putStrLn $ ("tokenizer settings:\n" :: Text) <> show (tokenizerSettingsCTTerm conns)

    let (ps, r) = parseG'' (tokenizeCTTerm conns . toS) (gCTTerm @'Concrete (tokenizeCTTerm conns . toS) conns) str
    putStrLn $ ("report:\n" :: Text) <> show r
    putStrLn $ ("parses:\n" :: Text) <> (T.intercalate "\n" $ map (show . (bimap identity unTok)) ps)
    putStrLn $ ("unique wf parses:\n" :: Text) <> (T.intercalate "\n" $ map (show . (bimap identity unTok)) $ S.toList $ S.fromList ps)

testM :: Text -> IO ()
testM str = do
    conns <- test

    putStrLn $ ("tokenizer settings:\n" :: Text) <> show (tokenizerSettingsCTTerm conns)

    let (ps, r) = parseG'' (tokenizeCTTerm conns . toS) (gCTTerm  @'Meta (tokenizeCTTerm conns . toS) conns) str
    putStrLn $ ("report:\n" :: Text) <> show r
    putStrLn $ ("parses:\n" :: Text) <> (T.intercalate "\n" $ map (show . (bimap identity unTok)) ps)
    putStrLn $ ("unique parses:\n" :: Text) <> (T.intercalate "\n" $ map (show . (bimap identity unTok)) $ S.toList $ S.fromList ps)
