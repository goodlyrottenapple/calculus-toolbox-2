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

module CTTerms.Typing.DescParser where

import           Lib.Prelude

import CTTerms.Core
import CTTerms.Parser.Core
import CTTerms.Parser.DescParser
import           Data.Aeson
import           Data.Map(Map)
import qualified Data.Map as M
import qualified Data.Text as T
import           Text.Earley(Report(..))




getTy :: (MapLike c a t , MonadState c m) => a -> m (Maybe t)
getTy a = gets (lookup a)

putTy :: (MapLike c a t , MonadState c m) => a -> t -> m ()
putTy a t = modify (add a t)



data FormulaLangTypeError a t = TypeMismatch {
    var :: a,
    ty1 :: t,
    ty2 :: t
} deriving (Show, Generic, ToJSON, Typeable)

instance (Typeable a, Typeable t, Show a, Show t) => 
    Exception (CTTerms.Typing.DescParser.FormulaLangTypeError a t)

isOfType :: (Eq t, Show a, Show t, Typeable a, Typeable t, ToJSON a, ToJSON t,
    MapLike c a t, MonadState c m, MonadThrowJSON m) => 
    a -> t -> m ()
isOfType a t = do
    ty <- getTy a
    case ty of
        Just t' -> 
            if t == t' then return () 
            else throw $ TypeMismatch a t t'
        Nothing -> putTy a t



data FormulaLangTy = SetVar | ElemVar | ListVar | Na deriving (Show, Generic, ToJSON, Typeable, Eq)



class MapLike c a t => TypeCheck c a t x | x -> a, x -> t where
    typeCheck :: (MonadState c m, MonadThrowJSON m) => x -> m ()


instance (Show a, Typeable a, ToJSON a, MapLike c a FormulaLangTy) =>
    TypeCheck c a FormulaLangTy (SetLang a a) where
    typeCheck (SVar s) =           s `isOfType` SetVar
    typeCheck (FSet es) =          forM_ es $ flip isOfType ElemVar
    typeCheck (SOp _ (Left l)) =   l `isOfType` ListVar
    typeCheck (SOp _ (Right ss)) = forM_ ss typeCheck


instance (Show a, Typeable a, ToJSON a, MapLike c a FormulaLangTy) =>
    TypeCheck c a FormulaLangTy (IntLang a a) where
    typeCheck (IntVal _) = return ()
    typeCheck (Card s) = typeCheck s


instance (Show a, Typeable a, ToJSON a, MapLike c a FormulaLangTy) => 
    TypeCheck c a FormulaLangTy (FormulaLang a a) where
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


instance (Show a, Typeable a, ToJSON a, MapLike c a FormulaLangTy) => 
    TypeCheck c a FormulaLangTy (CTTerms.Core.Type a a) where
    typeCheck (NVar (Just e)) = e `isOfType` ElemVar
    typeCheck (CType _ (CSetDecl t cs)) = do
        t `isOfType` SetVar
        forM_ cs typeCheck
    typeCheck (CType _ (FSetDecl es)) = forM_ es (`isOfType` ElemVar)
    typeCheck (CListType _ (Just l)) = l `isOfType` ListVar
    typeCheck _ = return ()


instance (Show a, Typeable a, ToJSON a, MapLike c a FormulaLangTy) => 
    TypeCheck c a FormulaLangTy [CTTerms.Core.Type a a] where
        typeCheck = mapM_ typeCheck


runTypeChecking :: forall m a t x. (TypeCheck (Map a t) a t x, MonadThrowJSON m) => x -> m ()
runTypeChecking trm = evalStateT (typeCheck @(Map a t) @a @t trm) emptyM



deriving instance Generic (Report Text [Text])
deriving instance ToJSON (Report Text [Text])


data DescParseTypingError = 
    DescParserError (Report Text [Text])
  | NotLevelUniform {
      conName :: (Token Text),
      outL :: Level,
      inL :: Level
    }
  | InvalidType {
      conName :: (Token Text),
      typ :: CTTerms.Core.Type (Token Text) (Token Text)
    }
  | MultipleTypeSigsSameName {
      conName1 :: (Token Text),
      conName2 :: (Token Text)
    }
  | DeclNotFound (Token Text)
  | NotPrefixSyntax (Token Text)
  | NotInfixSyntax (Token Text)
  | IncorrectNumberOfArgs {
  conName :: (Token Text)
  , expected :: Int
  , given :: Int
}
-- | AmbiguousCalcDescParse (Report P.String [P.String]) [[CalcFileParse]]
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

data DescParseTypingWarning = 
    NoParserOptsFound | NoSyntaxOptsFound


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
  , mod     :: [Text]
  , fixity  :: CTTerms.Parser.DescParser.Fixity
  , latex   :: Text
  , katex   :: Maybe Text
} deriving Show




data CalcDesc r = CalcDesc {
    conns  :: LevelList (ConnDescription (CTTerms.Core.Type Text Text)) 'Structure
  , mod    :: [Text]
  , rules  :: r
  -- , grammars :: LevelList ?? 'Structure
  -- , tokenizer :: Text -> [Token Text]
  -- , macros :: Map Text (Int,Text)
} deriving Show


checkTypeSig :: MonadThrowJSON m => [DescParse (Token Text)] -> (DescParse (Token Text)) -> m ()
checkTypeSig allTypeSigs TypeSig{..} = do
    let sameNameTySigs = filter (\(TypeSig n _ _) -> n == name) allTypeSigs
    case sameNameTySigs of
        [_] -> return ()
        ((TypeSig x _ _):(TypeSig y _ _):_) -> throw $ MultipleTypeSigsSameName x y
        _ -> undefined

    levelMatches (getLevel outTy) (map getLevel inTys)
    runTypeChecking $ outTy : inTys
    where
        levelMatches :: MonadThrowJSON m => Maybe Level -> [Maybe Level] -> m ()
        levelMatches Nothing _ = throw $ InvalidType name outTy -- cant have a type ... -> ... -> Name
        levelMatches l [] = return ()
        levelMatches l (Nothing:tys) = levelMatches l tys
        levelMatches (Just l) ((Just l'):tys) 
            | l == l' = levelMatches (Just l) tys
            | otherwise = throw $ NotLevelUniform name l l'
checkTypeSig _ _ = undefined

checkParserOpts :: MonadThrowJSON m => Map (Token Text) Int -> (DescParse (Token Text)) -> m ()
checkParserOpts declFuns ParserOpts{..}
    | name `M.member` declFuns = do
        let s = unTok name 
            noOfHoles = T.length $ T.filter (=='_') s

        unless (noOfHoles == (declFuns M.! name)) $ 
            throw IncorrectNumberOfArgs {
                conName = name
              , expected = declFuns M.! name
              , given = noOfHoles
            }
        case fixity of
            CTTerms.Parser.DescParser.Prefix _ ->
                when ("_" `T.isPrefixOf` s) $ throw $ NotPrefixSyntax name
            CTTerms.Parser.DescParser.Infix _ _ -> do
                unless ("_" `T.isPrefixOf` s && "_" `T.isSuffixOf` s && noOfHoles == 2) $ 
                    throw $ NotInfixSyntax name
            Mixfix _ -> return ()
    | otherwise = throw $ DeclNotFound name
checkParserOpts _ _ = undefined


checkSyntaxOpts :: MonadThrowJSON m => Map (Token Text) Int -> (DescParse (Token Text)) -> m ()
checkSyntaxOpts declFuns SyntaxOpts{..} 
    | name `M.member` declFuns = return ()
    | otherwise = throw $ DeclNotFound name
checkSyntaxOpts _ _ = undefined



checkTypeSigParserSyntaxOpts :: MonadThrowJSON m => [DescParse (Token Text)] -> m ()
checkTypeSigParserSyntaxOpts ds = do
    let typeSigs   = [x | x@(TypeSig{..})    <- ds]
        parserOpts = [x | x@(ParserOpts{..}) <- ds]
        syntaxOpts = [x | x@(SyntaxOpts{..}) <- ds]
        declFuns   = M.fromList $ map (\TypeSig{..} -> (name, length inTys)) typeSigs

    forM_ typeSigs   (checkTypeSig typeSigs)
    forM_ parserOpts (checkParserOpts declFuns)
    forM_ syntaxOpts (checkSyntaxOpts declFuns)




-- -- mkCalcDesc "." ["Lattice"] (Just "L") (Hidden [])
-- mkCalcDesc :: (MonadIO m, MonadThrowJSON m) => FilePath -> [Text] -> Maybe Text -> Visibilty Text -> Text -> m () --m (CalcDesc ())
-- mkCalcDesc xs = do
--     _ <- readFile'


--     where
--         readF file = do
--             exists <- liftIO $ doesFileExist $ file
--             unless exists $ throw $ FileDoesNotExist file
--             liftIO $ readFile' file

