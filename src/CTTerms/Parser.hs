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

module CTTerms.Parser where

import CTTerms.Core
import           Lib.Prelude
import qualified Prelude            as P

-- import           Data.Singletons
-- import Text.Parsec (CalculusDescParseError, parse, between, eof, try)
-- import Text.Parsec.Text (Parser)
-- import Text.Parsec.Char (oneOf, char, digit, string, letter, satisfy, spaces, endOfLine)

-- import Control.Applicative
-- import Control.Monad.Except(withExceptT)


-- import           Data.Aeson
import           Data.Char
import           Data.HashSet       (HashSet)
import qualified Data.HashSet       as HS
-- import qualified Data.Map           as M
-- import           Data.Set           (Set)
-- import qualified Data.Set           as S
-- import qualified Data.Text          as T
import           Text.Earley
-- import           Text.Earley.Mixfix

-- import           Text.Regex         (mkRegex, splitRegex)
-- import Data.Tree
import Data.String(IsString(..))
import Data.List(stripPrefix)
import           Data.Singletons

import Debug.Trace(trace)

debug a m = Debug.Trace.trace m a


-- type G t = forall r. Grammar r (Prod r P.String P.String t)

prefix :: P.String -> P.String -> Maybe P.String
prefix [] ys = Just ys
prefix (_:_) [] = Nothing
prefix (x:xs) (y:ys) | x == y = prefix xs ys
                     | otherwise = Nothing

newtype Row = Row { unRow :: Int } deriving (Eq, Show, Num)
newtype Col = Col { unCol :: Int } deriving (Eq, Show, Num)

data Token a = Token {
    tok :: a,
    rowStart :: Row,
    rowEnd :: Row,
    colStart :: Col,
    colEnd :: Col
} deriving Show

instance Eq a => Eq (Token a) where
    t1 == t2 = tok t1 == tok t2

instance IsString a => IsString (Token a) where
    fromString s = Token (fromString s) 0 0 0 0


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
incrBy (x:xs) = do
    modify (second (+1))
    incrBy xs

data TokenizerSettings = TokenizerSettings {
    reserved :: [P.String],
    comments :: [(P.String, P.String)],
    special :: P.String
}

tokenize' :: MonadState (Row, Col) m => TokenizerSettings -> P.String -> m [Token Text]
tokenize' _ "" = return []
tokenize' ts ('\n':xs) = do
    incrBy "\n"
    tokenize' ts xs
tokenize' ts (x:xs) 
    | x == ' ' || x == '\t' = do
    incrBy [x]
    tokenize' ts xs
tokenize' ts (startsWithRes' (comments ts) -> Just (start, end, s)) = do
    (row,col) <- get -- get the current position in text
    incrBy start -- move by the 'start' token
    (startRow,startCol) <- get -- get the end of the 'start' token
    let (comment, s') = firstOccurence end s -- look for the first occurence of the 'end' token
    incrBy comment
    (commentRow,commentCol) <- get
    -- if we found the 'end' tag, strip it from s' and add it to the token list
    -- otherwise we must have reached the end of the string, in which case, only return the 'start' tag
    -- and 'comment' tags, as s' must be empty.
    case stripPrefix end s' of
        Just s'' -> do
            incrBy end
            (endRow,endCol) <- get
            let sToken = Token (toS start) row startRow col startCol
                cToken = Token (toS comment) startRow commentRow startCol commentCol
                eToken = Token (toS end) commentRow endRow commentCol endCol

            liftM ([sToken, cToken, eToken] ++) $ tokenize' ts s''
        Nothing ->
            let sToken = Token (toS start) row startRow col startCol
                cToken = Token (toS comment) startRow commentRow startCol commentCol in
            return [sToken, cToken]
tokenize' ts (startsWithRes (reserved ts) -> Just (p, s')) = do
    (rowS,colS) <- get
    incrBy p
    (rowE,colE) <- get
    liftM (Token (toS p) rowS rowE colS colE :) $ tokenize' ts s'
tokenize' ts (x:xs)
  | x `HS.member` special' = do
    (rowS,colS) <- get
    incrBy [x]
    liftM (Token (toS [x]) rowS rowS colS (colS+1) :) $ tokenize' ts xs
  | otherwise             = do
    (rowS,colS) <- get
    incrBy [x]
    (as, bs) <- break' (`HS.member` special') xs
    (rowE,colE) <- get
    liftM (Token (toS (x:as)) rowS rowE colS colE:) $ tokenize' ts bs
  where
    special' = HS.fromList (special ts)



tokenizeDescFile :: P.String -> [Token Text]
tokenizeDescFile s = evalState (tokenize' (TokenizerSettings 
    ["|*", "::" , "->", "<=", ">=", "..."] 
    [(['"'], ['"']), (['-','-'], ['\n']), (['{','-'] , ['-', '}'])] 
    " \n(){}[],;|<>=:&-.") s) (1,1)


type G t = forall r. Grammar r (Prod r (Token Text) (Token Text) t)


satisfyT :: (t -> Bool) -> Prod r e (Token t) (Token t)
satisfyT f = satisfy (f . tok)


parseG :: G t -> Text -> Either (Report (Token Text) [Token Text]) t
parseG g t =
    case fullParses (parser $ g) $ tokenizeDescFile $ toS t of
        ([p] , _) -> Right p
        (_ , r) -> Left r

parseG' :: G t -> Text -> Either (Report Text [Text]) t
parseG' g t =
    case fullParses (parser $ g) $ tokenizeDescFile $ toS t of
        ([p] , _) -> Right $ p
        (_ , (Report p e u)) -> Left $ Report p (map tok e) (map tok u)





reservedSetLang :: HashSet Text
reservedSetLang = HS.fromList ["{" , "}" , "," , "&", "|", "|*", "<", "<=", "-"]

gSetLang :: G (SetLang (Token Text) (Token Text))
gSetLang = mdo
    sName  <- rule $ SVar <$> satisfyT (not . (`HS.member` reservedDecl))
    name   <- rule $ satisfyT (not . (`HS.member` reservedDecl))
    list   <- rule $ (:[]) <$> name
        <|> (:) <$> name <* namedToken "," <*> list
    fSet   <- rule $ (\xs -> FSet xs) <$> (namedToken "{" *> list <* namedToken "}")
    empty  <- rule $ (\_ -> FSet []) <$> namedToken "{" <* namedToken "}"
    bin    <- rule $ (\x op y -> SOp op (Right [x,y])) <$> setDecls <*> setOp <*> full
    list'  <- rule $ (:[]) <$> full
         <|> (:) <$> full <* namedToken "," <*> list'
    listOp <- rule $ (
            (\op x -> SOp op (Left x)) <$> setOp <*> name
        <|> (\op xs -> SOp op (Right xs)) <$> setOp <*>  (namedToken "[" *> list' <* namedToken "]")
        )
    setOp  <- rule $
            (\_ -> CTTerms.Core.Intersection) <$> namedToken "&"
        <|> (\_ -> Union)                     <$> namedToken "|"
        <|> (\_ -> DisjointUnion)             <$> namedToken "|*"
        <|> (\_ -> Difference)                <$> namedToken "-"
    let setDecls = empty <|> fSet <|> sName
    full   <- rule $ setDecls <|> listOp <|> bin
    return full


reservedIntLang :: HashSet Text
reservedIntLang = HS.fromList ["card" , "|"]

gIntLang :: G (IntLang (Token Text) (Token Text))
gIntLang = mdo
    slang  <- gSetLang
    intVal <- rule $ (IntVal . P.read . toS . tok) <$> satisfyT (all isDigit . (toS  :: Text -> P.String))
    card   <- rule $ Card <$> ((namedToken "card" *> slang) <|> (namedToken "|" *> slang <* namedToken "|"))
    return $ intVal <|> card



reservedFormulaLang :: HashSet Text
reservedFormulaLang = HS.fromList [":", "<", ">", "<=", ">=", "=", "~"] 
    `HS.union` reservedSetLang `HS.union` reservedIntLang

gFormulaLang :: G (FormulaLang (Token Text) (Token Text))
gFormulaLang = mdo
    -- sName <- rule $ (SVar . toS) <$> satisfy (not . (`HS.member` reservedDecl))
    name      <- rule $ satisfyT (not . (`HS.member` reservedDecl))
    intLang   <- gIntLang
    setLang   <- gSetLang
    member    <- rule $ Member <$> (name <* namedToken ":") <*> setLang
    compOp    <- rule $
            (\_ -> CTTerms.Core.Eq) <$> namedToken "="
        <|> (\_ -> CTTerms.Core.LT) <$> namedToken "<"
        <|> (\_ -> LTEq)            <$> namedToken "<="
        <|> (\_ -> CTTerms.Core.GT) <$> namedToken ">"
        <|> (\_ -> GTEq)            <$> namedToken ">="

    assertSet <- rule $ (\x op y -> BinSet op x y) <$> setLang <*> compOp <*> setLang
    assertInt <- rule $ (\x op y -> BinInt op x y) <$> intLang <*> compOp <*> intLang
    neg       <- rule $ Neg <$> (namedToken "~" *> full)

    let full = member <|> assertSet <|> assertInt
    return $ full


reservedType :: HashSet Text
reservedType = HS.fromList [":", "(", ")", "...", "where", ";", "List", "Name", "Term", "Formula", "Structure"] 
    `HS.union` reservedFormulaLang

gType :: G (CTTerms.Core.Type ((Token Text), [FormulaLang (Token Text) (Token Text)]) (Token Text))
gType = mdo
    name        <- rule $ satisfyT (not . (`HS.member` reservedDecl))
    nVar        <- rule $
            (NVar . Just) <$> (namedToken "(" *> name <* (namedToken ":" *> namedToken "Name" *> namedToken ")")) 
        <|> (\_ -> NVar Nothing) <$> namedToken "Name"
    level <- rule $
            (\_ -> Term) <$> namedToken "Term"
        <|> (\_ -> Formula) <$> namedToken "Formula"
        <|> (\_ -> Structure) <$> namedToken "Structure"
    assertLang  <- gFormulaLang
    assertList  <- rule $ (:[]) <$> assertLang
                       <|> (:) <$> assertLang <* namedToken ";" <*> assertList
 -- fSetList    <- rule $ 
 --         (:[]) <$> name
 --     <|> (:) <$> name <* namedToken "," <*> fSetList
 -- fSet        <- rule $ (\xs -> FSet xs) <$> (namedToken "{" *> fSetList <* namedToken "}")
    constraints <- rule $ 
            ((,[]) <$> name) -- Term x
     -- <|> ((\fs -> ("t", [AssertSet CTTerms.Core.Eq (SVar "t") fs])) <$> fSet) -- Term {a,b,c}
        <|> ((,) <$> (namedToken "(" *> name <* namedToken "where") <*> (assertList <* namedToken ")")) -- Term (x where ...)
    cType       <- rule $
            (\l -> CType l Nothing) <$> level
        <|> (\l ts -> CType l (Just ts)) <$> level <*> constraints

    cListType   <- rule $ 
            (\l -> CListType l Nothing) <$> ((namedToken "List" *> level) <|> (level <* namedToken "..."))
        <|> (\l n -> CListType l (Just n)) <$> ((namedToken "List" *> level) <|> (level <* namedToken "...")) <*> name

    return $ nVar <|> cType <|> cListType




reservedDecl :: HashSet Text
reservedDecl = HS.fromList ["::", "->", ":"] 
    `HS.union` reservedType

gDecl :: G (Decl (Token Text) (Token Text))
gDecl = mdo
    name     <- rule $ satisfyT (not . (`HS.member` reservedDeclSyntax))
    typeLang <- gType
    arr      <- rule $ ([],) <$> typeLang
        <|> (\x (xs,ty) -> (x:xs, ty)) <$> typeLang <* namedToken "->" <*> arr
    return $ (\n (ts , t) -> Decl n ts t) <$> name <*> (namedToken "::" *> arr)


reservedDeclSyntax :: HashSet Text
reservedDeclSyntax = HS.fromList ["{", "}", "infixl", "infixr", "prefix", "postfix"] 
    `HS.union` reservedDecl

-- gDeclSyntax :: G (DeclSyntax (Token Text))
-- gDeclSyntax = mdo
--     name <- rule $ satisfyT (not . (`HS.member` reservedDeclSyntax))
--     latexSyntax <- 




reservedLisp :: HashSet Text
reservedLisp = HS.fromList ["(", ")", "[", "]"] 


isLast :: [a] -> Bool
isLast [_] = True
isLast _ = False


gCTTermLisp :: forall l k. [HashSet Text] -> G (CTTerm l k () (Token Text))
gCTTermLisp lst@(P.head -> reserved) = mdo
    nm    <- rule $ satisfyT (not . (`HS.member` reservedLisp))
    conNm <- rule $ tok <$> satisfyT (`HS.member` reserved)
    lower <- gCTTermLisp @(Lower l) (P.tail lst)
    trm   <- rule $ 
            (if isLast lst then (Lift . Nm) <$> nm else Lift <$> lower)
        <|> (namedToken "(" *> exprT <* namedToken ")")
    -- list <- rule $ List <$> (namedToken "[" *> some exprT <* namedToken "]")
    con   <- rule $ (\n xs -> Con n xs ()) <$> (namedToken "(" *> conNm) <*> some exprT <* namedToken ")"
    let exprT = con <|> trm -- list <|> trm
    return exprT


