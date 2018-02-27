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

module CTTerms.Parser.Core(
    Token, joinT, unTok, 
    tokenize, TokenizerSettings(..), defaultTokenizerSettings, 
    G, parseG, parseG', satisfyT, var, 
    mkTable) where

-- import CTTerms.Core
import           Lib.Prelude
import qualified Prelude            as P

-- import           Data.Singletons
-- import Text.Parsec (CalculusDescParseError, parse, between, eof, try)
-- import Text.Parsec.Text (Parser)
-- import Text.Parsec.Char (oneOf, char, digit, string, letter, satisfy, spaces, endOfLine)

-- import Control.Applicative
-- import Control.Monad.Except(withExceptT)


import           Data.Aeson
import           Data.Char
import           Data.HashSet       (HashSet)
import qualified Data.HashSet       as HS
-- import qualified Data.Map           as M
-- import           Data.Set           (Set)
-- import qualified Data.Set           as S
import qualified Data.Text          as T
import           Text.Earley
import           Text.Earley.Mixfix(Associativity(..), Holey)

-- import           Text.Earley.Mixfix

-- import           Text.Regex         (mkRegex, splitRegex)
-- import Data.Tree
import Data.String(IsString(..))
import Data.List(stripPrefix)
-- import           Data.Singletons


prefix :: P.String -> P.String -> Maybe P.String
prefix [] ys = Just ys
prefix (_:_) [] = Nothing
prefix (x:xs) (y:ys) | x == y = prefix xs ys
                     | otherwise = Nothing

newtype Row = Row Int deriving (Eq, Show, Num, ToJSON, FromJSON)
newtype Col = Col Int deriving (Eq, Show, Num, ToJSON, FromJSON)

data Token a = Token {
    unTok :: a,
    rowStart :: Row,
    rowEnd :: Row,
    colStart :: Col,
    colEnd :: Col
} deriving (Generic, Show)

instance ToJSON a => ToJSON (Token a)

instance Eq a => Eq (Token a) where
    t1 == t2 = unTok t1 == unTok t2

instance Ord a => Ord (Token a) where
    compare t1 t2 = compare (unTok t1) (unTok t2)

instance IsString a => IsString (Token a) where
    fromString s = Token (fromString s) 0 0 0 0


joinT :: Semigroup a => Token a -> Token a -> Token a
joinT (Token t1 rS _ cS _) (Token t2 _ rE _ cE) = Token (t1 <> t2) rS rE cS cE



startsWithRes :: [P.String] -> P.String -> Maybe (P.String, P.String)
startsWithRes xs s = foldr (\p prev -> case prev of Just x -> Just x ; Nothing -> liftM (p,) $ prefix p s) Nothing xs


break' :: MonadState (Row, Col) m => (Char -> Bool) -> P.String -> m (P.String, P.String)
break' _ [] = return ([], [])
break' t s@(x:xs) 
    | t x = return ([], s)
    | otherwise = do
        if x == '\n' then modify (bimap (+1) (const 0)) else modify (second (+1))
        liftM (first (x:)) $ break' t xs


startsWithRes' :: [(P.String, P.String)] -> P.String -> Maybe (P.String, P.String, P.String)
startsWithRes' xs str = foldr (\(p,s) prev -> case prev of Just x -> Just x ; Nothing -> liftM (p,s,) $ prefix p str) Nothing xs


firstOccurence :: P.String -> P.String -> (P.String, P.String)
firstOccurence _ [] = ([], [])
firstOccurence pre s@(x:xs) | (Just s') <- prefix pre s = ([], pre ++ s')
                            | otherwise = first (x:) $ firstOccurence pre xs



incrBy :: MonadState (Row, Col) m => P.String -> m ()
incrBy "" = return ()
incrBy ('\n':xs) = do
    modify (bimap (+1) (const 1))
    incrBy xs
incrBy (_:xs) = do
    modify (second (+1))
    incrBy xs

data TokenizerSettings = TokenizerSettings {
    reserved :: [P.String]
  , comment :: [(P.String, P.String)]
  , block :: [(P.String, P.String)]
  , delim :: [Char]
  , special :: [Char]
}

defaultTokenizerSettings :: TokenizerSettings
defaultTokenizerSettings = TokenizerSettings {
    reserved = []
  , comment = []
  , block = []
  , delim = []
  , special = []
}

tokenize :: MonadState (Row, Col) m => TokenizerSettings -> P.String -> m [Token Text]
tokenize _ "" = return []
tokenize ts ('\n':xs) = do
    incrBy "\n"
    tokenize ts xs
tokenize ts (x:xs) 
    | x == ' ' || x == '\t' = do
    incrBy [x]
    tokenize ts xs
tokenize ts (startsWithRes' (comment ts) -> Just (start, end, s)) = do
    incrBy start -- move by the 'start' token
    let (com, s') = firstOccurence end s -- look for the first occurence of the 'end' token
    incrBy com
    -- if we found the 'end' tag, strip it from s' and add it to the token list
    -- otherwise we must have reached the end of the string, in which case, only return the 'start' tag
    -- and 'comment' tags, as s' must be empty.
    case stripPrefix end s' of
        Just s'' -> do
            incrBy end
            tokenize ts s''
        Nothing ->
            return []
tokenize ts (startsWithRes' (block ts) -> Just (start, end, s)) = do
    (row,col) <- get -- get the current position in text
    incrBy start -- move by the 'start' token
    (startRow,startCol) <- get -- get the end of the 'start' token
    let (blck, s') = firstOccurence end s -- look for the first occurence of the 'end' token
    incrBy blck
    (blckRow,blckCol) <- get
    -- if we found the 'end' tag, strip it from s' and add it to the token list
    -- otherwise we must have reached the end of the string, in which case, only return the 'start' tag
    -- and 'comment' tags, as s' must be empty.
    case stripPrefix end s' of
        Just s'' -> do
            incrBy end
            (endRow,endCol) <- get
            let sToken = Token (toS start) row startRow col startCol
                cToken = Token (toS blck) startRow blckRow startCol blckCol
                eToken = Token (toS end) blckRow endRow blckCol endCol

            liftM ([sToken, cToken, eToken] ++) $ tokenize ts s''
        Nothing ->
            let sToken = Token (toS start) row startRow col startCol
                cToken = Token (toS blck) startRow blckRow startCol blckCol in
            return [sToken, cToken]
tokenize ts (startsWithRes (reserved ts) -> Just (p, s')) = do
    (rowS,colS) <- get
    incrBy p
    (rowE,colE) <- get
    liftM (Token (toS p) rowS rowE colS colE :) $ tokenize ts s'
tokenize ts (x:xs)
  | x `HS.member` delim' = do
    (rowS,colS) <- get
    incrBy [x]
    (as, bs) <- break' (/= x) xs
    (rowE,colE) <- get
    liftM (Token (toS (x:as)) rowS rowE colS colE:) $ tokenize ts bs
  | x `HS.member` special' = do
    (rowS,colS) <- get
    incrBy [x]
    liftM (Token (toS [x]) rowS rowS colS (colS+1) :) $ tokenize ts xs
  | otherwise             = do
    (rowS,colS) <- get
    incrBy [x]
    (as, bs) <- break' (`HS.member` special') xs
    (rowE,colE) <- get
    liftM (Token (toS (x:as)) rowS rowE colS colE:) $ tokenize ts bs
  where
    special' = HS.fromList (special ts)
    delim' = HS.fromList (delim ts)



type G t = forall r. Grammar r (Prod r (Token Text) (Token Text) t)


parseG :: (Text -> [Token Text]) -> G t -> Text -> Either (Report (Token Text) [Token Text]) t
parseG tok g t =
    case fullParses (parser $ g) $ tok t of
        ([p] , _) -> Right p
        (_ , r) -> Left r

parseG' :: (Text -> [Token Text]) -> G t -> Text -> Either (Report Text [Text]) t
parseG' tok g t =
    case fullParses (parser $ g) $ tok t of
        ([p] , _) -> Right $ p
        (_ , (Report p e u)) -> Left $ Report p (map unTok e) (map unTok u)





satisfyT :: (t -> Bool) -> Prod r e (Token t) (Token t)
satisfyT f = satisfy (f . unTok)


var :: HashSet Text -> Prod r e (Token Text) (Token Text)
var reserved = satisfy (\t -> 
    not ((unTok t) `HS.member` reserved) &&
    T.length (unTok t) > 0 &&
    not (isDigit $ T.head (unTok t)))


-- parseG :: G t -> Text -> Either (Report (Token Text) [Token Text]) t
-- parseG g t =
--     case fullParses (parser $ g) $ tokenizeDescFile $ toS t of
--         ([p] , _) -> Right p
--         (_ , r) -> Left r

-- parseG' :: G t -> Text -> Either (Report Text [Text]) t
-- parseG' g t =
--     case fullParses (parser $ g) $ tokenizeDescFile $ toS t of
--         ([p] , _) -> Right $ p
--         (_ , (Report p e u)) -> Left $ Report p (map unTok e) (map unTok u)



holey :: P.String -> Holey (Token Text)
holey ""       = []
holey ('_':xs) = Nothing : holey xs
holey xs       = Just (Token (toS i) 0 0 0 0) : holey rest
  where (i, rest) = P.span (/= '_') xs



mkTable :: [[(P.String, Associativity, [expr] -> expr)]] -> [[(Holey (Prod r (Token Text) (Token Text) (Token Text)), Associativity, Holey (Token Text) -> [expr] -> expr)]]
mkTable [] = []
mkTable (x:xs) = mkRow x : mkTable xs
    where
        mkRow :: [(P.String, Associativity, [expr] -> expr)] -> [(Holey (Prod r (Token Text) (Token Text) (Token Text)), Associativity, Holey (Token Text) -> [expr] -> expr)]
        mkRow [] = []
        mkRow ((s, a, f):ys) = 
            let hs         = holey s
                hprod      = map (map namedToken) hs in
            (hprod, a, \_ -> f): mkRow ys
