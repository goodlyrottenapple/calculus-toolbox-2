{-|
Module      : Lib
Description : Lib's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Terms.Parsers where

import           Lib.Prelude
import qualified Prelude            as P
import           Terms
import           Rules.CutElimConditions
-- import Data.Singletons.TH
-- import GHC.TypeLits(SomeNat(..))
import           Data.Singletons
-- import Text.Parsec (CalculusDescParseError, parse, between, eof, try)
-- import Text.Parsec.Text (Parser)
-- import Text.Parsec.Char (oneOf, char, digit, string, letter, satisfy, spaces, endOfLine)

-- import Control.Applicative
-- import Control.Monad.Except(withExceptT)


-- import System.Environment
import           Data.Aeson
import           Data.Char
import           Data.HashSet       (HashSet)
import qualified Data.HashSet       as HS
import qualified Data.Map           as M
import           Data.Set           (Set)
import qualified Data.Set           as S
import qualified Data.Text          as T
import           Text.Earley
import           Text.Earley.Mixfix

import           Text.Regex         (mkRegex, splitRegex)
-- import Control.Monad.Catch


deriving instance Generic (Report P.String [P.String])
deriving instance ToJSON (Report P.String [P.String])

deriving instance Generic Associativity
deriving instance ToJSON Associativity


------------------------- Parsing of the Description file -------------------------

reservedCalcFile :: HashSet P.String
reservedCalcFile = HS.fromList ["(", ")", "{", "}", "|-", "default", "type" ,":", "->", "{#", "#}"]

data CalcFileParse = CalcTypeP Bool CalcType
                   | ConP Level Text [Maybe CalcType] (Maybe CalcType) (Text, Text.Earley.Mixfix.Associativity, Int, Text)
                   | Pragma [Char] [Text] deriving (Show, Generic, ToJSON)

grammarAssociativity :: Grammar r (Prod r P.String P.String Text.Earley.Mixfix.Associativity)
grammarAssociativity =
    rule $  (\_ -> LeftAssoc) <$> namedToken "LeftAssoc"
        <|> (\_ -> NonAssoc) <$> namedToken "NonAssoc"
        <|> (\_ -> RightAssoc) <$> namedToken "RightAssoc"



grammarConParams :: Grammar r (Prod r P.String P.String (Text, Text.Earley.Mixfix.Associativity, Int, Text))
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
    pragma <- rule $ (\name params -> Pragma (toS name) params) <$> (namedToken "{#" *> tname) <*> ((some tname) <* namedToken "#}")
    typ <- rule $ ((CalcTypeP False) . Type . toS) <$> (namedToken "type" *> satisfy (not . (`HS.member` reservedCalcFile)))
            <|> ((CalcTypeP True) . Type . toS) <$> (namedToken "default" *> namedToken "type" *> satisfy (not . (`HS.member` reservedCalcFile)))
            <?> "type"
    ftyp <- grammarFTypes "formula"
    styp <- grammarFTypes "structure"
    conParams <- grammarConParams
    fcon <- rule $ (\n (xs,t) par -> ConP FormulaL n xs t par) <$> (tname <* namedToken ":") <*> ftyp <*> conParams
    scon <- rule $ (\n (xs,t) par -> ConP StructureL n xs t par) <$> (tname <* namedToken ":") <*> styp <*> conParams
    return $ some (pragma <|> typ <|> fcon <|> scon)




tokenize :: P.String -> [P.String]
tokenize ""        = []
tokenize (' ':xs)  = tokenize xs
tokenize ('\n':xs) = tokenize xs
tokenize ('{':'#':xs) = "{#" : tokenize xs -- denotes a pragma
tokenize ('#':'}':xs) = "#}" : tokenize xs -- denotes a pragma
tokenize ('{':'-':xs)  = tokenize (comment xs) -- skip comments
  where
    comment ys = case map toS $ T.breakOn "-}" $ toS ys of
        (_,('-':'}':vs)) -> vs
        (_,vs)           -> vs
tokenize ('"':xs)  = as : tokenize bs
  where
    (as, bs) = brackets xs
    brackets ys = case break (=='"') ys of
        (us,('"':vs)) -> (us,vs)
        x             -> x
tokenize (x:xs)
  | x `HS.member` special = [x] : tokenize xs
  | otherwise             = (x:as) : tokenize bs
  where
    (as, bs) = break (`HS.member` special) xs
    special = HS.fromList "(){}, \n"



detokenize :: P.String -> Int -> Int -> (Int,Int)
detokenize ""           pos _        = (pos,pos)
detokenize (' ':xs)     pos nthToken = detokenize xs (pos+1) nthToken
detokenize ('\n':xs)    pos nthToken = detokenize xs (pos+1) nthToken
detokenize ('{':'#':xs) pos nthToken = detokenize xs (pos+2) (nthToken+1)
detokenize ('#':'}':xs) pos nthToken = detokenize xs (pos+2) (nthToken+1)
detokenize ('"':xs)     pos nthToken = case nthToken of
    0 -> (pos,fPos)
    _ -> detokenize bs (fPos+1) (nthToken-1)
  where
    (fPos, bs) = brackets xs
    brackets ys = case break (=='"') ys of
        (us,('"':vs)) -> (pos + length us + 1,vs)
        (us,vs)       -> (pos + length us,vs)
detokenize (x:xs)       pos nthToken
  | x `HS.member` special = case nthToken of
    0 -> (pos,pos+1)
    _ -> detokenize xs (pos+1) (nthToken-1)
  | otherwise             = case nthToken of
    0 -> (pos,fPos)
    _ -> detokenize bs (fPos+1) (nthToken-1)
  where
    (as, bs) = break (`HS.member` special) xs
    fPos = pos + length as
    special = HS.fromList "(){}, \n"

data CalculusDescParseError = CalcDescParserError (Report P.String [P.String])
                            | AmbiguousCalcDescParse (Report P.String [P.String]) [[CalcFileParse]]
                            | MultipleDefaultTypes
                            | NoTypesDeclared
                            | NoDefaultTypeDeclared
                            | SameNameConn Text
                            | SameParserSyntax Text Text
                            | IncorrectNoOfArgs {
                                connective :: Text,
                                expected :: Int,
                                noOfHoles :: Int
                              } -- the number of holes differs from the number of args expected
                            | TypesNotDeclared {
                                connective :: Text,
                                missingTypes :: Set CalcType
                              }
                            | ConnNameContainsNums Text
                            | InvalidPragma [Char] [Text]
                                deriving (Show, Generic, ToJSON, Typeable)

deriving instance Exception CalculusDescParseError

applyPragma :: MonadThrowJSON m => FinTypeCalculusDescription a -> ([Char] , [Text]) -> m (FinTypeCalculusDescription a)
applyPragma d@Description{..} ("MACRO" , [name, defn]) = return d{macros = M.insert ("\\" <> name) defn macros}
-- applyPragma d                 ('-':'-':_ , _) = return d -- a comment pragma
applyPragma _ (n, args) = throw $ InvalidPragma n args

mkFinTypeCalculusDescription :: MonadThrowJSON m => [CalcFileParse] -> m (FinTypeCalculusDescription ())
mkFinTypeCalculusDescription ps = do
    -- _ <- getPragmas ps
    defaultType <- onlyOneDefault ps
    let types = S.fromList $ extractTypes ps
        rules = ()
        prags = map (\(Pragma n args) -> (n, args)) $ filter (\x -> case x of { Pragma _ _ -> True ; _ -> False}) ps
    _ <- checkConns ps
    (formulaConns , structureConns) <- foldrM
        (\p (fCs,sCs) -> case p of {
            ConP FormulaL n ts t (ps',a,b,latex) -> do
                _ <- checkName n
                fC <- mkConnDescription defaultType types n ts t ps' a b latex
                return $ (fC:fCs,sCs)
          ; ConP StructureL n ts t (ps',a,b, latex) -> do
                _ <- checkName n
                sC <- mkConnDescription defaultType types n ts t ps' a b latex
                return $ (fCs,sC:sCs)
          ; _ -> return (fCs,sCs) }) ([],[]) ps
    foldM applyPragma Description{..} prags

    where
        macros = M.empty
        -- getPragmas prs = 
        --     let 
        --         prags = filter (\x -> case x of { Pragma _ _ -> True ; _ -> False}) prs
        --     in 
        --         if length prags == 0 then return () else throw $ FoundPragmas $ map (\(Pragma n args) -> (n, args)) prags

        -- onlyOneDefault :: [CalcFileParse] -> Except CalculusDescParseError CalcType
        onlyOneDefault prs = case calcTypes of
            []              -> throw NoTypesDeclared
            [CalcTypeP _ t] -> return t
            _ -> case filter (\x -> case x of {(CalcTypeP True _) -> True ; _ -> False}) calcTypes of
                [] -> throw NoDefaultTypeDeclared
                [CalcTypeP True t] -> return t
                _                  -> throw MultipleDefaultTypes

            where
                calcTypes = filter (\x -> case x of {(CalcTypeP _ _) -> True ; _ -> False}) prs

        extractTypes :: [CalcFileParse] -> [CalcType]
        extractTypes []                   = []
        extractTypes ((CalcTypeP _ t):xs) = t:extractTypes xs
        extractTypes (_:xs)               = extractTypes xs

        noOfHoles = T.foldr (\c acc -> if c == '_' then acc+1 else acc) 0
        -- checkConns :: [CalcFileParse] -> Except CalculusDescParseError ()
        checkConns = checkConns' S.empty M.empty
            where
                checkConns' _    _    [] = return ()
                checkConns' accN _    ((ConP _ n _ _ _):_)        | n `S.member` accN =
                    throw $ SameNameConn n
                checkConns' _    accS ((ConP _ n _ _ (s,_,_,_)):_)  | s `M.member` accS =
                    throw $
                        SameParserSyntax n (M.findWithDefault "error, this can't happen" s accS)
                checkConns' _ _ ((ConP _ n ts _ (s,_,_,_)):_) | length ts /= noOfHoles s =
                    throw $ IncorrectNoOfArgs n (length ts) (noOfHoles s)
                checkConns' accN accS ((ConP _ n _ _ (s,_,_,_)):xs) | otherwise =
                    checkConns' (S.insert n accN) (M.insert s n accS) xs
                checkConns' accN accS (_:xs) = checkConns' accN accS xs


        -- mkConnDescription :: CalcType -> Set CalcType ->
        --     Text -> [Maybe CalcType] -> Maybe CalcType -> Text -> Text.Earley.Mixfix.Associativity -> Int -> Text ->
        --     Except CalculusDescParseError (ConnDescription l)
        mkConnDescription defaultType types n ts t prs a b latex =
            if (S.fromList $ outType:inTypes) `S.isSubsetOf` types then
                return $ ConnDescription{..}
            else throw $ TypesNotDeclared name $ (S.fromList $ outType:inTypes) S.\\ types 
            where
                name = n
                inTypes = map (fromMaybe defaultType) ts
                outType = fromMaybe defaultType t
                assoc = a
                binding = b
                parserSyntax = prs
                latexSyntax = latex

        checkName name = if T.all (not . isDigit) name then return () else throw $ ConnNameContainsNums name


parseFinTypeCalculusDescription :: MonadThrowJSON m => Text -> m (FinTypeCalculusDescription ())
parseFinTypeCalculusDescription t =
    case fullParses (parser $ grammarFinTypeCalculusDesc) $ tokenize $ toS t of
        ([p] , _) -> mkFinTypeCalculusDescription p
        ([]  , r) -> throw $ CalcDescParserError r
        (xs   , r) -> throw $ AmbiguousCalcDescParse r xs -- this should hopefully not happen



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


mkhTable :: [ConnDescription l] -> [[(Holey (Prod r P.String P.String P.String), Text.Earley.Mixfix.Associativity)]]
mkhTable = (map snd) . sortBy (\(x,_) (y,_) -> compare x y) . M.toList . groupConns
    where
        groupConns :: [ConnDescription l] -> Map Int [(Holey (Prod r P.String P.String P.String), Text.Earley.Mixfix.Associativity)]
        groupConns [] = M.empty
        groupConns (ConnDescription{..}:xs) = M.alter (alterF parserSyntax assoc) binding $ groupConns xs

        alterF :: Text -> Text.Earley.Mixfix.Associativity -> Maybe [(Holey (Prod r P.String P.String P.String), Text.Earley.Mixfix.Associativity)] ->
            Maybe [(Holey (Prod r P.String P.String P.String), Text.Earley.Mixfix.Associativity)]
        alterF s a Nothing   = Just $ [(mkHoley s,a)]
        alterF s a (Just ls) = Just $ (mkHoley s,a):ls

        mkHoley :: Text -> Holey (Prod r P.String P.String P.String)
        mkHoley t = map (map namedToken) (holey $ toS t)


hTable :: Monad m => (x -> [ConnDescription l]) -> ReaderT x m [[(Holey (Prod r P.String P.String P.String), Text.Earley.Mixfix.Associativity)]]
hTable f = do
    d <- ask
    return $ mkhTable (f d)


lookupH :: Monad m => (x -> [ConnDescription l]) -> ReaderT x m (Map (Holey P.String) Text)
lookupH f = do
    d <- ask
    return $ M.fromList $ map (\ConnDescription{..} -> (holey $ toS parserSyntax, name)) (f d)


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




data TermParseError = TermParserError {
                        report :: Report P.String [P.String],
                        position :: (Int,Int)
                      }
                    | AmbiguousTermParse (Report P.String [P.String])
                    | AmbiguousRuleParse [Rule Text]
                    | TermUntypeable (TypeableError Text)
                    | RuleUntypeable Text 
                    | CutElimConditionViolated Int (Rule Text) deriving (Show, Generic, ToJSON)

instance Exception TermParseError



parseFormula :: (MonadReader (FinTypeCalculusDescription r) m , MonadThrowJSON m) =>
    CalcType -> Text -> m (Term 'FormulaL 'ConcreteK Text)
parseFormula typ inStr = do
    e <- ask
    case fullParses (parser $ runReaderT grammarTerm e) $ tokenize $ toS inStr of
        ([p] , _) -> tryTypeable p
        ([] , r@Report{..}) -> throw $ TermParserError r (detokenize (toS inStr) 0 (position-1))
        ((p:_) , _) -> tryTypeable p

    where
        tryTypeable parsed = do
            _ <- typeableC typ parsed
            return parsed


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


parseCDSeq :: (MonadReader (FinTypeCalculusDescription r) m , MonadThrowJSON m) =>
    Text -> m (DSequent 'ConcreteK Text)
parseCDSeq inStr = do
    e <- ask
    case fullParses (parser $ runReaderT grammarDSeq e) $ tokenize $ toS inStr of
        ([p] , _) -> tryTypeable p
        ([] , r@Report{..}) -> throw $ TermParserError r (detokenize (toS inStr) 0 (position-1))
        ((p:_) , _) -> tryTypeable p

    where
        tryTypeable parsed = do
            _ <- typeableCDSeq' M.empty parsed
            return parsed




-------------------------------- parsing of the rules ------------------------------------

grammarRule :: CalcMT x (Grammar r) (Prod r P.String P.String (Rule Text))
grammarRule = mdo
    let name = satisfy (not . (`HS.member` reservedCalcFile))
    rname   <- lift $ rule $ (\x -> (toS x , Nothing)) <$> name
            <|> (\n l -> (toS n , Just $ toS l)) <$> name <*> ((namedToken "(" *> name) <* namedToken ")")
    bar     <- lift $ rule $ satisfy (\x -> filter (/='-') x == "")
    dblBar  <- lift $ rule $ satisfy (\x -> filter (/='=') x == "")
    exprSeq <- grammarDSeq
    lift $ rule $ (\(n , l) c -> Rule n l [] c) <$> (bar *> rname) <*> exprSeq
            <|> (\ps (n , l) c -> Rule n l ps c) <$> some exprSeq <*> (bar *> rname) <*> exprSeq
            <|> (\p (n , l) c -> RevRule n l p c) <$> exprSeq <*> (bar *> rname <* bar) <*> exprSeq
            <|> (\p (n , l) c -> RevRule n l p c) <$> exprSeq <*> (bar *> bar *> rname) <*> exprSeq
            <|> (\p (n , l) c -> RevRule n l p c) <$> exprSeq <*> (dblBar *> rname) <*> exprSeq


splitRules :: P.String -> [P.String]
splitRules = filter (not . emptyString) . splitRegex (mkRegex "\n\n+")
    where
        emptyString :: P.String -> Bool
        emptyString x = x == "" || (S.fromList x) `S.isSubsetOf` (S.fromList " \n")


-- this version does not throw an error on ambiguous parse,
-- but choses the most general version of the rule
parseRules :: (MonadReader (FinTypeCalculusDescription r) m , MonadThrowJSON m) =>
    Text -> m [Rule Text]
parseRules str = parse $ (filter (\(_ , tokens) -> tokens /= []) . map (\x -> (x , tokenize x)) . splitRules . toS) str
    where
        parse :: (MonadReader (FinTypeCalculusDescription r) m , MonadThrowJSON m) =>
            [(P.String , [P.String])] -> m [Rule Text]
        parse [] = return []
        parse ((r,rTokens):rs) = do
            cd <- ask
            r' <- case fullParses (parser $ runReaderT grammarRule cd) $ rTokens of
                ([] , rep@Report{..}) -> throw $ TermParserError rep (detokenize (toS r) 0 (position-1))
                (prules@(r'':_) , _) -> do
                    prulesMetaMap <- (mapM typeableRule prules)
                    fixed <- zipWithM (\m r''' -> fixRule m r''') prulesMetaMap prules
                    case fixed of
                        []  -> throw $ RuleUntypeable $ ruleName r''
                        [p] -> return p
                        ps' -> throw $ AmbiguousRuleParse ps'

            checkCondition 1 conditionC1 r'
            rs' <- parse rs
            return $ r':rs'

        checkCondition n cFun r = 
            if (not $ cFun r) then throw $ CutElimConditionViolated n r else return ()

