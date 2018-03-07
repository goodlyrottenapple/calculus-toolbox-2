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

module CTTerms.Parser.DescParser(
    Binding(..), CTTerms.Parser.DescParser.Fixity(..), DescParse(..), Visibility(..),
    tokenizeDescParse,
    gSetLang, gIntLang, gFormulaLang, gType, gDescParse, gDescParseList) where

import CTTerms.Core
import CTTerms.Parser.Core

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
-- import qualified Data.Text          as T
import           Text.Earley
import           Text.Earley.Mixfix(Associativity(..), mixfixExpressionSeparate)

-- import           Text.Earley.Mixfix

-- import           Text.Regex         (mkRegex, splitRegex)
-- import Data.Tree
-- import Data.String(IsString(..))
-- import Data.List(stripPrefix)
-- import           Data.Singletons

-- import Debug.Trace(trace)

-- debug :: a -> P.String -> a
-- debug a m = Debug.Trace.trace m a



newtype Binding = Binding Int deriving (Eq, Show, Ord, Generic, ToJSON)

deriving instance Generic Text.Earley.Mixfix.Associativity
deriving instance ToJSON Text.Earley.Mixfix.Associativity

data Fixity = 
    Prefix Binding
  | Infix {
        binding :: Binding
      , assoc :: Text.Earley.Mixfix.Associativity
    }
  | Mixfix Binding deriving (Show, Generic, ToJSON)


data PragmaTy = Macro deriving (Show, Generic, ToJSON)

data Visibility a = Visible [a] | Hidden [a] deriving (Show, Functor, Generic, ToJSON)

data DescParse a = 
    TypeSig {
        name :: a
      , inTys :: [CTTerms.Core.Type a a] 
      , outTy :: CTTerms.Core.Type a a
    }
  | ParserOpts {
        name :: a
      , fixity :: CTTerms.Parser.DescParser.Fixity
    }
  | SyntaxOpts {
        name :: a
      , latex :: a
      , katex :: Maybe a
    } 
  | Pragma {
        pragmaType :: PragmaTy
      , params :: [a]
    } 
  | Import {
        moduleName :: [a]
      , as :: Maybe a
      , visible :: Visibility a
      -- , renaming :: [(a,a)]
    } deriving (Show, Generic, ToJSON)

-- deriving instance ToJSON a => ToJSON (DescParse a)


instance Functor DescParse where
    fmap f (TypeSig n ts t) = TypeSig (f n) (map (bimap f f) ts) (bimap f f t) --(map (bimap (bimap f (map (bimap f f))) f) ts) (bimap (bimap f (map (bimap f f))) f t)
    fmap f (ParserOpts s fx) = ParserOpts (f s) fx
    fmap f (SyntaxOpts n ltx ktx) = SyntaxOpts (f n) (f ltx) (map f ktx)
    fmap f (Pragma t ps) = Pragma t (map f ps)
    fmap f (Import n a v) = Import (map f n) (map f a) (map f v) --(map (bimap f f) r)




tokenizeDescParse :: P.String -> [Token Text]
tokenizeDescParse = flip evalState (1,1) . tokenize defaultTokenizerSettings {
        reserved = ["||*", "|*", "||", "&&", "->", "<=", ">=", "...", "/\\", "\\/"] 
      , comment = [("--", "\n")]
      , block = [("\"", "\""), ("{-#" , "#-}")]
      , special = " \n(){}[],;|<>=:&-~./\\"
    }


parseDescParse :: Text -> Either (Report (Token Text) [Token Text]) (DescParse (Token Text))
parseDescParse = parseG (tokenizeDescParse . toS) gDescParse




reservedSetLang :: HashSet Text
reservedSetLang = HS.fromList ["(", ")", "{" , "}" , "," , "&&", "||", "||*", "&", "|", "|*", "-"]



gSetLang :: G (SetLang (Token Text) (Token Text))
gSetLang = mdo
    -- sName <- rule $ SVar <$> var reservedDescParse
    name  <- rule $ var reservedDescParse
    list  <- rule $ (:[]) <$> name
        <|> (:) <$> name <* namedToken "," <*> list

    empty <- rule $ (\_ -> FSet []) <$> namedToken "{" <* namedToken "}"
    fSet  <- rule $ (\xs -> FSet xs) <$> (namedToken "{" *> list <* namedToken "}")
 
    setOp <- rule $
            (\_ -> CTTerms.Core.Intersection) <$> namedToken "&&"
        <|> (\_ -> Union)                     <$> namedToken "||"
        <|> (\_ -> DisjointUnion)             <$> namedToken "||*"

    bigOp <- rule $ (\op x -> SOp op (Left x)) <$> setOp <*> name
    atom  <- rule $ 
                bigOp <|> empty <|> fSet <|> (SVar <$> name)
            <|> (namedToken "(" *> expr <* namedToken ")")

    expr  <- mixfixExpressionSeparate table atom
    return expr
    where
        table = mkTable
            [ [("_&_",  LeftAssoc, SOp CTTerms.Core.Intersection . Right), 
               ("_|_",  LeftAssoc, SOp Union . Right),
               ("_|*_", LeftAssoc, SOp DisjointUnion . Right)]
            , [("_-_",  LeftAssoc, SOp Difference . Right)]
            ]

reservedIntLang :: HashSet Text
reservedIntLang = HS.fromList ["card" , "|"]

gIntLang :: G (IntLang (Token Text) (Token Text))
gIntLang = mdo
    slang  <- gSetLang
    intVal <- rule $ (IntVal . P.read . toS . unTok) <$> satisfyT (all isDigit . (toS  :: Text -> P.String))
    card   <- rule $ Card <$> ((namedToken "card" *> slang) <|> (namedToken "|" *> slang <* namedToken "|"))
    expr   <- rule $ (namedToken "(" *> expr <* namedToken ")") <|> intVal <|> card
    return expr


gCompOp :: G a -> G (CompOp, a, a)
gCompOp gLang = mdo
    atom   <- gLang
    compOp <- rule $
            (\_ -> CTTerms.Core.Eq) <$> namedToken "="
        <|> (\_ -> CTTerms.Core.LT) <$> namedToken "<"
        <|> (\_ -> LTEq)            <$> namedToken "<="
        <|> (\_ -> CTTerms.Core.GT) <$> namedToken ">"
        <|> (\_ -> GTEq)            <$> namedToken ">="

    rule $ (\x op y -> (op, x, y)) <$> atom <*> compOp <*> atom
    -- return expr


reservedFormulaLang :: HashSet Text
reservedFormulaLang = HS.fromList [":", "<", ">", "<=", ">=", "=", "~", "/\\", "\\/", "->", "if", "then", "else"] 
    `HS.union` reservedSetLang `HS.union` reservedIntLang

gFormulaLang :: G (FormulaLang (Token Text) (Token Text))
gFormulaLang = mdo
    name    <- rule $ var reservedDescParse
    setLang <- gSetLang
    member  <- rule $ Member <$> (name <* namedToken ":") <*> setLang
    
    setOp   <- gCompOp gSetLang
    intOp   <- gCompOp gIntLang
    binSet  <- rule $ (uncurry3 BinSet) <$> setOp
    binInt  <- rule $ (uncurry3 BinInt) <$> intOp

    atom    <- rule $ 
            member <|> binInt <|> binSet
        <|> (namedToken "(" *> expr <* namedToken ")")

    expr    <- mixfixExpressionSeparate table atom
    return expr
    where
        table = mkTable
            [ [("_->_",          RightAssoc, \[x,y]   -> BinForm Impl x y)]
            , [("_/\\_",         LeftAssoc,  \[x,y]   -> BinForm And x y), 
               ("_\\/_",         LeftAssoc,  \[x,y]   -> BinForm Or x y)]
            , [("~_",            RightAssoc, \[x]     -> Neg x)]
            , [("if_then_else_", NonAssoc,   \[x,y,z] -> 
                                                BinForm And 
                                                    (BinForm Impl x y) 
                                                    (BinForm Impl (Neg x) z))]
            ]


reservedType :: HashSet Text
reservedType = HS.fromList [":", "(", ")", "...", "where", ";", "List", "Name", "Term", "Formula", "Structure"] 
    `HS.union` reservedFormulaLang

gType :: G (CTTerms.Core.Type (Token Text) (Token Text))
gType = mdo
    name        <- rule $ var reservedDescParse
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
    list    <- rule $ 
         (:[]) <$> name
     <|> (:) <$> name <* namedToken "," <*> list
    -- fSet        <- rule $ (\xs -> FSet xs) <$> (namedToken "{" *> fSetList <* namedToken "}")
    constraints <- rule $ 
            (,[]) <$> name -- Term x
        <|> (,) <$> (namedToken "(" *> name <* namedToken "where") <*> (assertList <* namedToken ")") -- Term (x where ...)
    cType       <- rule $
            (\l -> CType l CTTerms.Core.Any) <$> level
        <|> (\l ts -> CType l (uncurry CSetDecl ts)) <$> level <*> constraints
        <|> (\l ts -> CType l (FSetDecl ts)) <$> level <*> (namedToken "{" *> list <* namedToken "}") -- Term {a,b,c}

    cListType   <- rule $ 
            (\l -> CListType l Nothing) <$> ((namedToken "List" *> level) <|> (level <* namedToken "..."))
        <|> (\l n -> CListType l (Just n)) <$> ((namedToken "List" *> level) <|> (level <* namedToken "...")) <*> name

    return $ nVar <|> cType <|> cListType



reservedDescParse :: HashSet Text
reservedDescParse = HS.fromList ["->", ":", "{", "}", "infixl", "infixr", "prefix", "mixfix", "=", "syntax", "import", "renaming", "hiding"] 
    `HS.union` reservedType


gDescParse :: G (DescParse (Token Text))
gDescParse = mdo
    name <- rule $ 
            var reservedDescParse
        <|> namedToken "(" *> parserSyntax <* namedToken ")"
    parserSyntax <- rule $
            satisfy (/= ")") -- <* namedToken ("::" :: Token Text)
        <|> joinT <$> satisfy (/= ")") <*> parserSyntax
    typeLang <- gType
    arr      <- rule $ ([],) <$> typeLang
        <|> (\x (xs,ty) -> (x:xs, ty)) <$> typeLang <* namedToken "->" <*> arr
    typeSig <- rule $ (\n (ts , t) -> TypeSig n ts t) <$> (name <* namedToken ":") <*> arr

    intVal <- rule $ (P.read . toS . unTok) <$> satisfyT (all isDigit . (toS  :: Text -> P.String))
    parserOpts <- rule $
            (\n b -> ParserOpts n (CTTerms.Parser.DescParser.Infix (Binding b) LeftAssoc)) <$> (namedToken "infixl" *> name) <*> intVal
        <|> (\n b -> ParserOpts n (CTTerms.Parser.DescParser.Infix (Binding b) RightAssoc)) <$> (namedToken "infixr" *> name) <*> intVal
        <|> (\n b -> ParserOpts n (CTTerms.Parser.DescParser.Prefix (Binding b))) <$> (namedToken "prefix" *> name) <*> intVal
        <|> (\n b -> ParserOpts n (Mixfix (Binding b))) <$> (namedToken "mixfix" *> name) <*> intVal

    latexSyntax <- rule $ 
            (,Nothing) <$> (Text.Earley.list ["latex", "=", "\""] *> satisfy (const True) <* namedToken "\"")
        <|> (\x y -> (x,Just y)) <$> (Text.Earley.list ["latex", "=", "\""] *> satisfy (const True) <* Text.Earley.list ["\"", ",", "katex", "=", "\""]) <*> (satisfy (const True) <* namedToken "\"")
        <|> (\x y -> (x,Just y)) <$> (Text.Earley.list ["katex", "=", "\""] *> satisfy (const True) <* Text.Earley.list ["\"", ",", "latex", "=", "\""]) <*> (satisfy (const True) <* namedToken "\"")
    -- syntax op "aaa"
    -- syntax op { latex = "aaa" }
    -- syntax op { latex = "aaa" , katex = "bbb" }
    -- syntax op { katex = "aaa" , latex = "bbb" }
    syntaxOpts <- rule $ 
            (\n x -> SyntaxOpts n x Nothing) <$> (namedToken "syntax" *> name <* namedToken "\"") <*> (satisfy (const True) <* namedToken "\"")
        <|> uncurry . SyntaxOpts <$> (namedToken "syntax" *> name <* namedToken "{") <*> latexSyntax <* namedToken "}"
    
    pragma <- rule $ 
            (\x y -> Pragma Macro [x,y]) <$> 
                (Text.Earley.list ["{-#" , "MACRO"] *> (var reservedDescParse)) <*> 
                (namedToken "\"" *> satisfy (const True) <* namedToken "\"")

    modName <- rule $
            (:[]) <$> satisfy (const True)
        <|> (:) <$> (satisfy (const True) <* namedToken ".") <*> modName

    nameList <- rule $
            (:[]) <$> name
        <|> (:) <$> (name <* namedToken ",") <*> nameList

    import1 <- rule $
            (\n -> Import n Nothing (Hidden [])) <$> (namedToken "import" *> modName)
    import2 <- rule $
            (\n vs -> Import n Nothing (Visible vs)) <$> 
                (namedToken "import" *> modName) <*> 
                (namedToken "(" *> nameList <* namedToken ")")
        <|> (\n vs as -> Import n (Just as) (Visible vs)) <$> 
            (namedToken "import" *> modName) <*> 
            (namedToken "(" *> nameList <* namedToken ")") <*>
            (namedToken "as" *> var reservedDescParse)
    import3 <- rule $
            (\n hs -> Import n Nothing (Hidden hs)) <$> 
            (namedToken "import" *> modName) <*> 
            (Text.Earley.list ["hiding" , "("] *> nameList <* namedToken ")")
        <|> (\n as hs -> Import n (Just as) (Hidden hs)) <$> 
            (namedToken "import" *> modName) <*> 
            (namedToken "as" *> var reservedDescParse) <*> 
            (Text.Earley.list ["hiding" , "("] *> nameList <* namedToken ")")
        <|> (\n hs as -> Import n (Just as) (Hidden hs)) <$> 
            (namedToken "import" *> modName) <*> 
            (Text.Earley.list ["hiding" , "("] *> nameList <* namedToken ")") <*>
            (namedToken "as" *> var reservedDescParse)

    let imports = import1 <|> import2 <|> import3
    return $ typeSig <|> parserOpts <|> syntaxOpts <|> pragma <|> imports




gDescParseList :: G [DescParse (Token Text)]
gDescParseList = mdo
    descP <- gDescParse
    return $ some descP

-- reservedLisp :: HashSet Text
-- reservedLisp = HS.fromList ["(", ")", "[", "]"] 


-- isLast :: [a] -> Bool
-- isLast [_] = True
-- isLast _ = False


-- gCTTermLisp :: forall l k. [HashSet Text] -> G (CTTerm l k () (Token Text))
-- gCTTermLisp lst@(P.head -> reserved) = mdo
--     nm    <- rule $ satisfyT (not . (`HS.member` reservedLisp))
--     conNm <- rule $ unTok <$> satisfyT (`HS.member` reserved)
--     lower <- gCTTermLisp @(Lower l) (P.tail lst)
--     trm   <- rule $ 
--             (if isLast lst then (Lift . Nm) <$> nm else Lift <$> lower)
--         <|> (namedToken "(" *> exprT <* namedToken ")")
--     -- list <- rule $ List <$> (namedToken "[" *> some exprT <* namedToken "]")
--     con   <- rule $ (\n xs -> Con n xs ()) <$> (namedToken "(" *> conNm) <*> some exprT <* namedToken ")"
--     let exprT = con <|> trm -- list <|> trm
--     return exprT


