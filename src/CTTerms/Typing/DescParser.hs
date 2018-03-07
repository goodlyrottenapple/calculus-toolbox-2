{-# LANGUAGE DeriveAnyClass        #-}
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
{-# LANGUAGE LambdaCase #-}

module CTTerms.Typing.DescParser where

import           Lib.Prelude

import CTTerms.Core
import CTTerms.Parser.Core
import CTTerms.Parser.DescParser
import           Data.Aeson
import           Data.Map(Map)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as S
import           Text.Earley(Report(..), fullParses, parser)
import System.FilePath.Posix((</>))
import qualified System.FilePath.Posix as FP
import System.Directory(doesFileExist)




data FileIOError = FileDoesNotExist FilePath deriving (Show, Generic, ToJSON, Typeable)

instance Exception FileIOError


data ErrorLocation = ErrorLocation {
    currentModule :: Module,
    row :: Row,
    col :: Col
} deriving (Show, Generic, ToJSON, Typeable)


mkErrorLocation :: Module -> Token a -> ErrorLocation
mkErrorLocation m t = ErrorLocation m (rowStart t) (colStart t)

data DescParseTypingError = 
    DescParserError (Report (Token Text) [Token Text])
  | NotLevelUniform {
      errorLocation :: ErrorLocation,
      conName :: Text,
      outL :: Level,
      inL :: Level
    }
  | InvalidType {
      errorLocation :: ErrorLocation,
      conName :: Text,
      typ :: CTTerms.Core.Type Text Text
    }
  | MultipleTypeSigsSameName {
      errorLocation1 :: ErrorLocation,
      errorLocation2 :: ErrorLocation,
      conName1 :: Text,
      conName2 :: Text
    }
  | DeclNotFound  {
      errorLocation :: ErrorLocation
    , conName :: Text
    }
  | NotPrefixSyntax {
      errorLocation :: ErrorLocation
    , conName :: Text
    }
  | NotInfixSyntax {
      errorLocation :: ErrorLocation
    , conName :: Text
    }
  | IncorrectNumberOfArgs {
        errorLocation :: ErrorLocation,
        conName :: Text
      , expected :: Int
      , given :: Int
    }
  | AmbiguousDescParse (Report (Token Text) [Token Text]) [[DescParse (Token Text)]]
  | CircularDependencyError {
      errorLocation :: ErrorLocation -- The current place in the chain the module was imported
    , circularImport :: Module -- The import which already appeared
    , circularImportModule :: Module -- The previous place in the chain the module was imported
    }
  | DuplicateDefn {
      errorLocation1 :: ErrorLocation
    , errorLocation2 :: ErrorLocation
    , conName :: Text
    , defnType :: DefnType
    }
   | DuplicateImport {
      errorLocation1 :: ErrorLocation
    , errorLocation2 :: ErrorLocation
    , moduleName :: Module
    }
   | ModuleDoesNotContain {
       moduleName :: Module
     , names :: Set Text
    }
-- | MultipleDefaultTypes
-- | NoTypesDeclared
-- | NoDefaultTypeDeclared
-- | SameNameConn Text
-- | SameParserSyntax Text Text Text
-- | IncorrectNoOfArgs {
--     connective :: Text,
--     expected :: Int,
--     noOfHoles :: Int
--   } -- the number of holes differs from the number of args expected
-- | TypesNotDeclared {
--     connective :: Text,
--     missingTypes :: Set CalcType
--   }
-- | ConnNameContainsNums Text
-- | InvalidPragma [Char] [Text]
    deriving (Show, Generic, ToJSON, Typeable)

data DefnType = TypeSigD | ParserOptsD | SyntaxOptsD | PragmaD | ImportD deriving (Show, Generic, ToJSON, Typeable)

-- data DescParseTypingWarning = 
--     NoParserOptsFound | NoSyntaxOptsFound


instance Exception DescParseTypingError

data LevelList a (l :: Level) where
    One :: a -> LevelList a 'Term
    Cons :: a -> LevelList a (Lower l) -> LevelList a l 

deriving instance Show a => Show (LevelList a l)

head :: LevelList a l -> a
head (One a) = a
head (Cons a _) = a


type family NotTerm (l :: Level) where
    NotTerm 'Term = 'False
    NotTerm l = 'True

tail :: NotTerm l ~ 'True => LevelList a l -> LevelList a (Lower l)
tail (Cons _ xs) = xs

mkLevelList :: a -> a -> a -> LevelList a 'Structure
mkLevelList a b c = Cons a $ Cons b $ One c

data ConnDescription t = ConnDescription {
    name    :: Text
  , inTypes :: [t]
  , outType :: t
  , originalModule :: Module
  , as :: Maybe Text
  , fixity  :: CTTerms.Parser.DescParser.Fixity
  , latex   :: Maybe Text
  , katex   :: Maybe Text
} deriving Show




data CalcDesc r = CalcDesc {
    conns  :: LevelList (ConnDescription (CTTerms.Core.Type Text Text)) 'Structure
  , mod    :: Module
  , rules  :: r
  -- , macros :: Map Text (Int,Text)
} deriving Show


newtype Module = Module [Text] deriving (Show, Generic, ToJSON, Typeable, Ord, Eq)





data FormulaLangTy = SetVar | ElemVar | ListVar | Na deriving (Show, Generic, ToJSON, Typeable, Eq)


getTy :: (MapLike c a t , MonadState c m) => a -> m (Maybe t)
getTy a = gets (lookup a)

putTy :: (MapLike c a t , MonadState c m) => a -> t -> m ()
putTy a t = modify (add a t)



data FormulaLangTypeError a t = TypeMismatch {
    errorLocation :: ErrorLocation,
    varName :: a,
    type1 :: t,
    type2 :: t
} deriving (Show, Generic, ToJSON, Typeable)

instance (Typeable a, Typeable t, Show a, Show t) => 
    Exception (CTTerms.Typing.DescParser.FormulaLangTypeError a t)

isOfType :: (Eq t, Show a, Show t, Typeable a, Typeable t, ToJSON a, ToJSON t,
    MapLike c (Token a) t, MonadState c m, MonadReader Module m, MonadThrowJSON m) => 
    (Token a) -> t -> m ()
isOfType a t = do
    ty <- getTy a
    currentM <- ask
    case ty of
        Just t' -> 
            if t == t' then return () 
            else throw TypeMismatch {
                errorLocation = mkErrorLocation currentM a
              , varName = (unTok a) 
              , type1 = t
              , type2 = t'
            }
        Nothing -> putTy a t




class MapLike c a t => TypeCheck c a t x | x -> a, x -> t where
    typeCheck :: (MonadState c m, MonadReader Module m, MonadThrowJSON m) => x -> m ()


instance (Show a, Typeable a, ToJSON a, MapLike c (Token a) FormulaLangTy) =>
    TypeCheck c (Token a) FormulaLangTy (SetLang (Token a) (Token a)) where
    typeCheck (SVar s) =           s `isOfType` SetVar
    typeCheck (FSet es) =          forM_ es $ flip isOfType ElemVar
    typeCheck (SOp _ (Left l)) =   l `isOfType` ListVar
    typeCheck (SOp _ (Right ss)) = forM_ ss typeCheck


instance (Show a, Typeable a, ToJSON a, MapLike c (Token a) FormulaLangTy) =>
    TypeCheck c (Token a) FormulaLangTy (IntLang (Token a) (Token a)) where
    typeCheck (IntVal _) = return ()
    typeCheck (Card s) = typeCheck s


instance (Show a, Typeable a, ToJSON a, MapLike c (Token a) FormulaLangTy) => 
    TypeCheck c (Token a) FormulaLangTy (FormulaLang (Token a) (Token a)) where
    typeCheck (Member e s) = do
        e `isOfType` ElemVar
        typeCheck s
    typeCheck (BinSet _ s1 s2) = do
        typeCheck s1
        typeCheck s2
    typeCheck (BinInt _ i1 i2) = do
        typeCheck i1
        typeCheck i2    
    typeCheck (BinForm _ f1 f2) = do
        typeCheck f1
        typeCheck f2
    typeCheck (Neg f) = typeCheck f


instance (Show a, Typeable a, ToJSON a, MapLike c (Token a) FormulaLangTy) => 
    TypeCheck c (Token a) FormulaLangTy (CTTerms.Core.Type (Token a) (Token a)) where
    typeCheck (NVar (Just e)) = e `isOfType` ElemVar
    typeCheck (CType _ (CSetDecl t cs)) = do
        t `isOfType` SetVar
        forM_ cs typeCheck
    typeCheck (CType _ (FSetDecl es)) = forM_ es (`isOfType` ElemVar)
    typeCheck (CListType _ (Just l)) = l `isOfType` ListVar
    typeCheck _ = return ()


instance (Show a, Typeable a, ToJSON a, MapLike c (Token a) FormulaLangTy) => 
    TypeCheck c (Token a) FormulaLangTy [CTTerms.Core.Type (Token a) (Token a)] where
        typeCheck = mapM_ typeCheck


runTypeChecking :: forall m a t x. (TypeCheck (Map a t) a t x, MonadReader Module m, MonadThrowJSON m) => x -> m ()
runTypeChecking trm = evalStateT (typeCheck @(Map a t) @a @t trm) emptyM



checkTypeSig :: (MonadReader Module m, MonadThrowJSON m) => DescParse (Token Text) -> m ()
checkTypeSig TypeSig{..} = do
    levelMatches (getLevel outTy) (map getLevel inTys)
    runTypeChecking $ outTy : inTys
    where
        levelMatches :: (MonadReader Module m , MonadThrowJSON m) => Maybe Level -> [Maybe Level] -> m ()
        levelMatches Nothing _ = do
            m <- ask
            throw $ InvalidType {
              errorLocation = mkErrorLocation m name,
              conName = unTok name,
              typ = bimap unTok unTok outTy
            } -- cant have a type ... -> ... -> Name
        levelMatches _ [] = return ()
        levelMatches l (Nothing:tys) = levelMatches l tys
        levelMatches (Just l) ((Just l'):tys) 
            | l == l' = levelMatches (Just l) tys
            | otherwise = do
                m <- ask
                throw $ NotLevelUniform {
                  errorLocation = mkErrorLocation m name,
                  conName = unTok name,
                  outL = l,
                  inL = l'
                }
checkTypeSig _ = return ()


checkParserOpts :: (MonadReader Module m , MonadThrowJSON m) => Map (Token Text) Int -> DescParse (Token Text) -> m ()
checkParserOpts declFuns ParserOpts{..}
    | name `M.member` declFuns = do
        let s = unTok name 
            noOfHoles = T.length $ T.filter (=='_') s
        m <- ask
        unless (noOfHoles == (declFuns M.! name)) $ 
            throw IncorrectNumberOfArgs {
                errorLocation = mkErrorLocation m name,
                conName = unTok name
              , expected = declFuns M.! name
              , given = noOfHoles
            }
        case fixity of
            CTTerms.Parser.DescParser.Prefix _ ->
                when ("_" `T.isPrefixOf` s) $ throw $ NotPrefixSyntax {
                  errorLocation = mkErrorLocation m name
                , conName = unTok name
                } 
            CTTerms.Parser.DescParser.Infix _ _ -> do
                unless ("_" `T.isPrefixOf` s && "_" `T.isSuffixOf` s && noOfHoles == 2) $ 
                    throw $ NotInfixSyntax {
                  errorLocation = mkErrorLocation m name
                , conName = unTok name
                } 
            Mixfix _ -> return ()
    | otherwise = do
        m <- ask
        throw $ DeclNotFound {
          errorLocation = mkErrorLocation m name
        , conName = unTok name
        } 
checkParserOpts _ _ = return ()


checkSyntaxOpts :: (MonadReader Module m , MonadThrowJSON m) => Set (Token Text) -> DescParse (Token Text) -> m ()
checkSyntaxOpts declFuns SyntaxOpts{..} 
    | name `S.member` declFuns = return ()
    | otherwise = do
        m <- ask
        throw $ DeclNotFound {
          errorLocation = mkErrorLocation m name
        , conName = unTok name
        } 
checkSyntaxOpts _ _ = return ()


checkTypeSigAndParserSyntaxOpts :: (MonadReader Module m, MonadThrowJSON m) => 
    [DescParse (Token Text)] -> m ()
checkTypeSigAndParserSyntaxOpts ds = do
    let declFuns   = M.fromList $ [(name, length inTys) | TypeSig{..} <- ds]
        declFunSet = S.fromList $ [name                 | TypeSig{..} <- ds]

    forM_ ds $ \x -> do
        checkTypeSig x 
        checkParserOpts declFuns x
        checkSyntaxOpts declFunSet x


throwDuplicateDefnError :: (MonadThrowJSON m, MonadReader Module m) =>
    Token a -> Token b -> Text -> DefnType -> m ()
throwDuplicateDefnError l1 l2 n dt = do
    currentM <- ask
    throw $ DuplicateDefn {
          errorLocation1 = mkErrorLocation currentM l1
        , errorLocation2 = mkErrorLocation currentM l2
        , conName = n
        , defnType = dt
        }

checkDuplicates :: (MonadReader Module m, MonadThrowJSON m) => [DescParse (Token Text)] -> m ()
checkDuplicates [] = return ()
checkDuplicates (x@TypeSig{}:xs) 
    | Just x' <- find (\case
        y@TypeSig{} -> (CTTerms.Parser.DescParser.name x) == (CTTerms.Parser.DescParser.name y)
        _           -> False) xs = 
        let l1 = (CTTerms.Parser.DescParser.name x)
            l2 = (CTTerms.Parser.DescParser.name x') in
            throwDuplicateDefnError l1 l2 (unTok l1) TypeSigD
checkDuplicates (x@ParserOpts{}:xs) 
    | Just x' <- find (\case 
        y@ParserOpts{} -> (CTTerms.Parser.DescParser.name x) == (CTTerms.Parser.DescParser.name y)
        _              -> False) xs = 
        let l1 = (CTTerms.Parser.DescParser.name x)
            l2 = (CTTerms.Parser.DescParser.name x') in
            throwDuplicateDefnError l1 l2 (unTok l1) ParserOptsD
checkDuplicates (x@SyntaxOpts{}:xs) 
    | Just x' <- find (\case
        y@SyntaxOpts{} -> (CTTerms.Parser.DescParser.name x) == (CTTerms.Parser.DescParser.name y)
        _              -> False) xs = 
        let l1 = (CTTerms.Parser.DescParser.name x)
            l2 = (CTTerms.Parser.DescParser.name x') in
            throwDuplicateDefnError l1 l2 (unTok l1) SyntaxOptsD
checkDuplicates (x@Import{}:xs) 
    | Just x' <- find (\case
        y@Import{} -> (CTTerms.Parser.DescParser.moduleName x) == (CTTerms.Parser.DescParser.moduleName y)
        _          -> False) xs = do
        currentM <- ask
        let (Just headOfModuleName1) = Lib.Prelude.head (CTTerms.Parser.DescParser.moduleName x)
            (Just headOfModuleName2) = Lib.Prelude.head (CTTerms.Parser.DescParser.moduleName x')
        throw $ DuplicateImport {
          errorLocation1 = mkErrorLocation currentM headOfModuleName1
        , errorLocation2 = mkErrorLocation currentM headOfModuleName2
        , CTTerms.Typing.DescParser.moduleName = Module $ map unTok $ CTTerms.Parser.DescParser.moduleName x
        }
checkDuplicates (_:xs) = checkDuplicates xs


readCalcDescFile :: (MonadIO m, MonadThrowJSON m) => FilePath -> [FilePath] -> m Text
readCalcDescFile basePath modulePathList = do
    let modulePath = foldr (</>) "" modulePathList
        fullPath = basePath </> modulePath FP.<.> ".calc"
    exists <- liftIO $ doesFileExist $ fullPath
    unless exists $ throw $ FileDoesNotExist fullPath
    readFile' fullPath


parseCalcDesc :: MonadThrowJSON m => Text -> m [DescParse (Token Text)]
parseCalcDesc t = do
    case fullParses (parser $ gDescParseList) $ tokenizeDescParse $ toS t of
        ([p] , _) -> return p
        ([] , r) -> throw $ DescParserError r
        (xs , r) -> throw $ AmbiguousDescParse r xs



checkCircularDependencies :: (MonadReader Module m, MonadThrowJSON m, MonadState (Map Module Module) m) => 
    [DescParse (Token Text)] -> m ()
checkCircularDependencies [] = return ()
checkCircularDependencies (Import{..}:xs) = do
    visited <- get
    currentM <- ask
    let importM = Module (map unTok moduleName)
        (Just headOfModuleName) = Lib.Prelude.head moduleName
    when (importM `M.member` visited) $ throw $ CircularDependencyError  {
      errorLocation = mkErrorLocation currentM headOfModuleName
    , circularImport = importM
    , circularImportModule = (visited M.! importM)
    }

    modify (M.insert importM currentM)
    checkCircularDependencies xs
checkCircularDependencies (_:xs) = checkCircularDependencies xs



-- filterByVisibilityRemovingImports :: (MonadReader Module m, MonadThrowJSON m) => 
--     Visibility Text -> [DescParse (Token Text)] -> m [DescParse (Token Text)]
-- filterByVisibilityRemovingImports v ds = do
--     let names = S.fromList [unTok name | TypeSig{..} <- ds]
--         v' = getSetFromVisible v

--     currentM <- ask
--     unless (v' `S.isSubsetOf` names) $ throw ModuleDoesNotContain {
--         moduleName = currentM
--       , names = S.difference v' names
--     }
--     return $ case v of
--         Hidden _  -> filterDescParse (\n ns ->  (unTok n) `S.member` ns) v' ds
--         Visible _ -> filterDescParse (\n ns ->  not ((unTok n) `S.member` ns)) v' ds
--     where
--         getSetFromVisible (Hidden  a) = S.fromList a
--         getSetFromVisible (Visible a) = S.fromList a

--         filterDescParse _ _ [] = []
--         filterDescParse c hs (TypeSig{..}:xs)    | c name hs = filterDescParse c hs xs
--         filterDescParse c hs (ParserOpts{..}:xs) | c name hs = filterDescParse c hs xs
--         filterDescParse c hs (SyntaxOpts{..}:xs) | c name hs = filterDescParse c hs xs
--         filterDescParse c hs (Import{}:xs) = filterDescParse c hs xs
--         filterDescParse c hs (x:xs) = x : filterDescParse c hs xs

-- mkCalcDesc "." ["Core", "Lattice"] (Just "L") (Hidden [])
mkCalcDesc :: (MonadIO m, MonadThrowJSON m, StringConv s FilePath, StringConv s Text, MonadState (Map Module Module) m) => 
    FilePath -> [s] -> Maybe s -> Visibility s -> m () --m (CalcDesc ())
mkCalcDesc basePath modulePathList _ visibility = do
    -- open calc file
    rawCalc <- readCalcDescFile basePath (map toS modulePathList)

    -- parse into DescParse
    parsedCalc <- parseCalcDesc rawCalc

    -- verify that DescParse is (somewhat) well formed
    inCurrentModule $ checkTypeSigAndParserSyntaxOpts parsedCalc
    inCurrentModule $ checkDuplicates parsedCalc

    -- add the current module to the list of open modules, to prevent a module importing itself
    modify (M.insert (Module $ map toS modulePathList) (Module $ map toS modulePathList))

    -- check whether imports are circular
    let imports = [x | x@Import{} <- parsedCalc]
    inCurrentModule $ checkCircularDependencies imports


    -- recursively load imported files
    importedCalcs <- mapM (\Import{..} -> mkCalcDesc basePath moduleName as visible) imports

    -- generate con defns


    -- merge??


    -- parse rules??


    -- apply visibility, etc.
    -- filteredParsedCalc <- inCurrentModule $ filterByVisibilityRemovingImports (map toS visibility) parsedCalc

    return ()

    where
        inCurrentModule :: ReaderT Module m a -> m a
        inCurrentModule f = runReaderT f (Module (map toS modulePathList)) 
