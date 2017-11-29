{-|
Module      : Lib
Description : Lib's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE FlexibleInstances      #-}

module Terms.Parsers where

import Terms
import Lib.Prelude
import qualified Prelude as P
-- import Data.Singletons.TH
import GHC.TypeLits(SomeNat(..))
import Data.Singletons
-- import Text.Parsec (CalculusDescParseError, parse, between, eof, try)
-- import Text.Parsec.Text (Parser)
-- import Text.Parsec.Char (oneOf, char, digit, string, letter, satisfy, spaces, endOfLine)

-- import Control.Applicative
-- import Control.Monad.Except(withExceptT)


-- import System.Environment
import Data.Char
import Text.Earley
import Text.Earley.Mixfix
import Data.HashSet(HashSet)
import qualified Data.HashSet as HS
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Set(Set)
import qualified Data.Set as S
import Data.Aeson

import Text.Regex (splitRegex, mkRegex)

deriving instance Generic (Report P.String [P.String])
deriving instance ToJSON (Report P.String [P.String])


------------------------- Parsing of the Description file -------------------------

reservedCalcFile :: HashSet P.String
reservedCalcFile = HS.fromList ["(", ")", "{", "}", "|-", "default", "type" ,":", "->"]

data CalcFileParse = CalcTypeP Bool CalcType 
                   | ConP Level Text [Maybe CalcType] (Maybe CalcType) (Text, Associativity, Int, Text) deriving Show

grammarAssociativity :: Grammar r (Prod r P.String P.String Associativity)
grammarAssociativity =
    rule $  (\_ -> LeftAssoc) <$> namedToken "LeftAssoc"
        <|> (\_ -> NonAssoc) <$> namedToken "NonAssoc"
        <|> (\_ -> RightAssoc) <$> namedToken "RightAssoc" 


grammarConParams :: Grammar r (Prod r P.String P.String (Text, Associativity, Int, Text))
grammarConParams = mdo
    parserStr <- rule $ toS <$> satisfy (not . (`HS.member` reservedCalcFile))
    assoc <- grammarAssociativity
    binding <- rule $ P.read <$> satisfy (foldr (\c b -> isDigit c && b) True)
    rule $ (,,,) <$> (namedToken "(" *> parserStr <* namedToken ",") <*> (assoc <* namedToken ",") <*> (binding <* namedToken ",") <*> (parserStr <* namedToken ")")


grammarFTypes :: P.String -> Grammar r (Prod r P.String P.String ([Maybe CalcType] , Maybe CalcType))
grammarFTypes name = mdo
    k <- rule $ toS <$> satisfy (not . (`HS.member` reservedCalcFile))
    t <- rule $ (\_ -> Nothing) <$> namedToken name
            <|> (Just . Type) <$> (namedToken name *> namedToken "{" *> k) <* namedToken "}"
    arr <- rule $ ([],) <$> t
        <|> (\x (xs,ty) -> (x:xs, ty)) <$> t <* namedToken "->" <*> arr
    return arr
        


grammarFinTypeCalculusDesc :: Grammar r (Prod r P.String P.String [CalcFileParse])
grammarFinTypeCalculusDesc = mdo
    tname <- rule $ toS <$> satisfy (not . (`HS.member` reservedCalcFile))
    typ <- rule $ ((CalcTypeP False) . Type . toS) <$> (namedToken "type" *> satisfy (not . (`HS.member` reservedCalcFile)))
            <|> ((CalcTypeP True) . Type . toS) <$> (namedToken "default" *> namedToken "type" *> satisfy (not . (`HS.member` reservedCalcFile)))
            <?> "type"
    ftyp <- grammarFTypes "formula"
    styp <- grammarFTypes "structure"
    conParams <- grammarConParams 
    fcon <- rule $ (\n (xs,t) par -> ConP FormulaL n xs t par) <$> (tname <* namedToken ":") <*> ftyp <*> conParams
    scon <- rule $ (\n (xs,t) par -> ConP StructureL n xs t par) <$> (tname <* namedToken ":") <*> styp <*> conParams
    return $ some (typ <|> fcon <|> scon)
    



tokenize :: P.String -> [P.String]
tokenize ""        = []
tokenize (' ':xs)  = tokenize xs
tokenize ('\n':xs) = tokenize xs
tokenize ('"':xs)  = as : tokenize bs
  where
    (as, bs) = brackets xs
    brackets ys = case break (=='"') ys of
        (us,('"':vs)) -> (us,vs)
        x -> x
tokenize (x:xs)
  | x `HS.member` special = [x] : tokenize xs
  | otherwise             = (x:as) : tokenize bs
  where
    (as, bs) = break (`HS.member` special) xs
    special = HS.fromList "(){}, \n"


data CalculusDescParseError = CalcDescParserError (Report P.String [P.String])
                            | AmbiguousCalcDescParse (Report P.String [P.String]) 
                            | MultipleDefaultTypes 
                            | SameNameConn Text
                            | SameParserSyntax Text Text
                            | IncorrectNoOfArgs Text Int Int -- the number of holes differs from the number of args expected
                            | TypesNotDeclared (Set CalcType) deriving (Show, Generic, ToJSON)

mkFinTypeCalculusDescription :: [CalcFileParse] -> Except CalculusDescParseError (FinTypeCalculusDescription ())
mkFinTypeCalculusDescription ps = do
    defaultType <- onlyOneDefault ps
    let types = S.fromList $ extractTypes ps
        rules = ()
    _ <- checkConns ps
    (formulaConns , structureConns) <- foldrM 
        (\p (fCs,sCs) -> case p of {
            ConP FormulaL n ts t (ps',a,b,latex) -> do
                fC <- mkConnDescription defaultType types n ts t ps' a b latex
                return $ (fC:fCs,sCs)
          ; ConP StructureL n ts t (ps',a,b, latex) -> do
                sC <- mkConnDescription defaultType types n ts t ps' a b latex
                return $ (fCs,sC:sCs)
          ; _ -> return (fCs,sCs) }) ([],[]) ps
    return $ Description{..}

    where
        onlyOneDefault :: [CalcFileParse] -> Except CalculusDescParseError CalcType
        onlyOneDefault prs = case filter (\x -> case x of {(CalcTypeP True _) -> True ; _ -> False}) prs of
            [CalcTypeP True t] -> return t
            _ -> throwError MultipleDefaultTypes

        extractTypes :: [CalcFileParse] -> [CalcType]
        extractTypes [] = []
        extractTypes ((CalcTypeP _ t):xs) = t:extractTypes xs
        extractTypes (_:xs) = extractTypes xs

        noOfHoles = T.foldr (\c acc -> if c == '_' then acc+1 else acc) 0
        checkConns :: [CalcFileParse] -> Except CalculusDescParseError ()
        checkConns = checkConns' S.empty M.empty
            where
                checkConns' _    _    [] = return ()
                checkConns' accN _    ((ConP _ n _ _ _):_)        | n `S.member` accN = 
                    throwError $ SameNameConn n
                checkConns' _    accS ((ConP _ n _ _ (s,_,_,_)):_)  | s `M.member` accS = 
                    throwError $ 
                        SameParserSyntax n (M.findWithDefault "error, this can't happen" s accS)
                checkConns' accN accS ((ConP _ n ts _ (s,_,_,_)):xs) | length ts /= noOfHoles s = 
                    throwError $ IncorrectNoOfArgs n (length ts) (noOfHoles s)
                checkConns' accN accS ((ConP _ n _ _ (s,_,_,_)):xs) | otherwise = 
                    checkConns' (S.insert n accN) (M.insert s n accS) xs
                checkConns' accN accS (_:xs) = checkConns' accN accS xs


        mkConnDescription :: CalcType -> Set CalcType -> 
            Text -> [Maybe CalcType] -> Maybe CalcType -> Text -> Associativity -> Int -> Text -> 
            Except CalculusDescParseError (ConnDescription l)
        mkConnDescription defaultType types n ts t prs a b latex = 
            if (S.fromList $ outType:inTypes) `S.isSubsetOf` types then 
                return $ ConnDescription{..}
            else throwError $ TypesNotDeclared $ types S.\\ (S.fromList $ outType:inTypes)
            where
                connName = n
                inTypes = map (fromMaybe defaultType) ts
                outType = fromMaybe defaultType t
                assoc = a
                binding = b
                parserSyntax = prs
                latexSyntax = latex


parseFinTypeCalculusDescription :: Text -> Except CalculusDescParseError (FinTypeCalculusDescription ())
parseFinTypeCalculusDescription t = 
    case fullParses (parser $ grammarFinTypeCalculusDesc) $ tokenize $ toS t of
        ([p] , _) -> mkFinTypeCalculusDescription p
        ([]  , r) -> throwError $ CalcDescParserError r
        (_   , r) -> throwError $ AmbiguousCalcDescParse r -- this should hopefully not happen



----------------------- parsing of the terms ----------------------------


holey :: P.String -> Holey P.String
holey ""       = []
holey ('_':xs) = Nothing : holey xs
holey xs       = Just i : holey rest
  where (i, rest) = P.span (/= '_') xs



grammarMeta :: SingI l => [Text] -> CalcMT x (Grammar r) (Prod r P.String P.String (Term l 'MetaK Text))
grammarMeta nPrefixes = mdo
    reserved <- mixfixParts
    lift $ rule $ (Meta . toS) <$> satisfy (\x -> 
        not (x `HS.member` reserved) &&
        foldr (\p b -> not (T.isPrefixOf p $ T.toLower $ toS x) && b) True nPrefixes)


mkhTable :: [ConnDescription l] -> [[(Holey (Prod r P.String P.String P.String), Associativity)]]
mkhTable = (map snd) . sortBy (\(x,_) (y,_) -> compare x y) . M.toList . groupConns
    where
        groupConns :: [ConnDescription l] -> Map Int [(Holey (Prod r P.String P.String P.String), Associativity)]
        groupConns [] = M.empty
        groupConns (ConnDescription{..}:xs) = M.alter (alterF parserSyntax assoc) binding $ groupConns xs

        alterF :: Text -> Associativity -> Maybe [(Holey (Prod r P.String P.String P.String), Associativity)] -> 
            Maybe [(Holey (Prod r P.String P.String P.String), Associativity)]
        alterF s a Nothing = Just $ [(mkHoley s,a)]
        alterF s a (Just ls) = Just $ (mkHoley s,a):ls

        mkHoley :: Text -> Holey (Prod r P.String P.String P.String)
        mkHoley t = map (map namedToken) (holey $ toS t)


hTable :: Monad m => (x -> [ConnDescription l]) -> ReaderT x m [[(Holey (Prod r P.String P.String P.String), Associativity)]]
hTable f = do
    d <- ask
    return $ mkhTable (f d)


lookupH :: Monad m => (x -> [ConnDescription l]) -> ReaderT x m (Map (Holey P.String) Text)
lookupH f = do
    d <- ask
    return $ M.fromList $ map (\ConnDescription{..} -> (holey $ toS parserSyntax, connName)) (f d)


mixfixParts :: Monad m => CalcMT x m (HashSet P.String)
mixfixParts = do
    Description{..} <- ask
    return $ HS.fromList $ 
        [s | ys <- map (\ConnDescription{..} -> holey $ toS parserSyntax) formulaConns   , Just s <- ys] ++ 
        [s | ys <- map (\ConnDescription{..} -> holey $ toS parserSyntax) structureConns , Just s <- ys] ++ ["(", ")", "|-"]


class TermGrammar l k where
    grammarTerm :: CalcMT x (Grammar r) (Prod r P.String P.String (Term l k Text))

instance TermGrammar 'FormulaL 'MetaK where
    grammarTerm = mdo
        tableF      <- hTable formulaConns
        lookupF     <- lookupH formulaConns
        -- metaA   <- grammarMeta ["f_","s_"]
        -- metaF   <- grammarMeta ["a_","at_","s_"]
        reserved    <- mixfixParts

        base        <- lift $ rule $ (Meta . toS) <$> satisfy (not . (`HS.member` reserved))
        atomF       <- lift $ rule $ Lift <$> base
                        <|> namedToken "(" *> exprF <* namedToken ")"
        exprF       <- lift $ mixfixExpression tableF atomF (mkCon lookupF)
        return exprF


instance TermGrammar 'FormulaL 'ConcreteK where
    grammarTerm = mdo
        tableF      <- hTable formulaConns
        lookupF     <- lookupH formulaConns
        reserved    <- mixfixParts

        base        <- lift $ rule $ (Base . toS) <$> satisfy (not . (`HS.member` reserved))
        atomF       <- lift $ rule $ Lift <$> base
                        <|> namedToken "(" *> exprF <* namedToken ")"
        exprF       <- lift $ mixfixExpression tableF atomF (mkCon lookupF)
        return exprF


instance TermGrammar 'StructureL 'MetaK where
    grammarTerm = mdo
        tableS  <- hTable structureConns
        lookupS <- lookupH structureConns

        exprF   <- grammarTerm
        atomS   <- lift $ rule $ Lift <$> exprF
                    <|> namedToken "(" *> exprS <* namedToken ")"
        exprS   <- lift $ mixfixExpression tableS atomS (mkCon lookupS)
        return exprS

instance TermGrammar 'StructureL 'ConcreteK where
    grammarTerm = mdo
        tableS  <- hTable structureConns
        lookupS <- lookupH structureConns

        exprF   <- grammarTerm
        atomS   <- lift $ rule $ Lift <$> exprF
                    <|> namedToken "(" *> exprS <* namedToken ")"
        exprS   <- lift $ mixfixExpression tableS atomS (mkCon lookupS)
        return exprS




data TermParseError = TermParserError (Report P.String [P.String])
                    | AmbiguousTermParse (Report P.String [P.String]) 
                    | AmbiguousRuleParse [Rule Text]
                    | TermUntypeable (TypeableError Text)
                    | RuleUntypeable Text deriving (Show, Generic, ToJSON)

parseTerm :: forall l k x. (TermGrammar l k) => Text -> 
    CalcMErr x TermParseError (Term l k Text)
parseTerm inStr = do
    e <- ask
    case fullParses (parser $ runReaderT grammarTerm e) $ tokenize $ toS inStr of
        ([p] , _) -> return p
        ([] , r) -> throwError $ TermParserError r
        (_ , r) -> throwError $ AmbiguousTermParse r



grammarDSeq :: TermGrammar 'StructureL k => 
    CalcMT x (Grammar r) (Prod r P.String P.String (DSequent k Text))
grammarDSeq = mdo
    Description{..} <- ask
    typ     <- lift $ rule $ (Type . toS) <$> satisfy 
                (\x -> (Type $ toS x) `S.member` types)
                <?> "type of sequent"
    exprS   <- grammarTerm
    lift $ rule $ DSeq <$> (exprS <* namedToken "|-") <*> (namedToken "{" *> typ <* namedToken "}") <*> exprS
                    <|> (\x y -> DSeq x defaultType y) <$> exprS <* namedToken "|-" <*> exprS



liftExcept :: CalcMErr x e a -> (e -> e') -> CalcMErr x e' a
liftExcept m f = do
    env <- ask
    case runCalcErr env m of 
        Left err -> throwError $ f err
        Right x -> return x

parseCDSeq :: Text -> CalcMErr x TermParseError (DSequent 'ConcreteK Text)
parseCDSeq inStr = do
    e <- ask
    case fullParses (parser $ runReaderT grammarDSeq e) $ tokenize $ toS inStr of
        ([p] , _) -> do
            _ <- typeableCDSeq' M.empty p`liftExcept` TermUntypeable
            return p
        ([] , r) -> throwError $ TermParserError r
        (_ , r) -> throwError $ AmbiguousTermParse r



-------------------------------- parsing of the rules ------------------------------------

grammarRule :: CalcMT x (Grammar r) (Prod r P.String P.String (Rule Text))
grammarRule = mdo
    rname   <- lift $ rule $ toS <$> satisfy (not . (`HS.member` reservedCalcFile))
    bar     <- lift $ rule $ satisfy (\x -> filter (/='-') x == "")
    exprSeq <- grammarDSeq
    lift $ rule $ (\n c -> Rule n [] c) <$> (bar *> rname) <*> exprSeq
            <|> (\ps n c -> Rule n ps c) <$> some exprSeq <*> (bar *> rname) <*> exprSeq




splitRules :: P.String -> [P.String]
splitRules = filter (not . emptyString) . splitRegex (mkRegex "\n\n+")
    where 
        emptyString :: P.String -> Bool
        emptyString = (\x -> x == "" || (S.fromList x) `S.isSubsetOf` (S.fromList " \n"))


parseRulesStrict :: Text -> CalcMErr x TermParseError [Rule Text]
parseRulesStrict inStr = do undefined
    -- e <- ask
    -- parse e $ (splitRules . toS) inStr
    -- where
    --     parse :: FinTypeCalculusDescription x -> [P.String] -> CalcMErr x TermParseError [Rule Text]
    --     parse _ [] = return []
    --     parse e (r:rs) = do
    --         r' <- case fullParses (parser $ runReaderT grammarRule e) $ tokenize $ r of
    --             ([] , rep) -> throwError $ TermParserError rep
    --             (ps@(Rule{..}:_) , _)   -> do
    --                 ps' <-  filterTypeable typeableRule ps
    --                 case ps' of
    --                     []  -> throwError $ RuleUntypeable name
    --                     [p] -> return p
    --                     _   -> throwError $ AmbiguousRuleParse ps'
    --         rs' <- parse e rs
    --         return $ r':rs'


-- this version does not throw an error on ambiguous parse, 
-- but choses the most general version of the rule
parseRules :: Text -> CalcMErr x TermParseError [Rule Text]
parseRules inStr = do undefined
    -- e <- ask
    -- parse e $ (splitRules . toS) inStr
    -- where
    --     parse :: FinTypeCalculusDescription x -> [P.String] -> CalcMErr x TermParseError [Rule Text]
    --     parse _ [] = return []
    --     parse e (r:rs) = do
    --         r' <- case fullParses (parser $ runReaderT grammarRule e) $ tokenize $ r of
    --             ([] , rep) -> throwError $ TermParserError rep
    --             (ps@(Rule{..}:_) , _)   -> do
    --                 ps' <-  filterTypeable typeableRule ps
    --                 case sort ps' of
    --                     []    -> throwError $ RuleUntypeable name
    --                     [p]   -> return p
    --                     (p:_) -> return p
    --         rs' <- parse e rs
    --         return $ r':rs'




testP :: IO (Either TermParseError [Rule Text])
testP = do
    cd <- readFile "DEAK.calc"
    let (Right calcDesc) = runExcept $ parseFinTypeCalculusDescription cd
    r <- readFile "DEAK.rules"
    return $ runCalcErr calcDesc $ parseRules r

