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
{-# LANGUAGE TemplateHaskell #-}

module CTTerms.Core where

-- import qualified Data.Text          as T
import           Lib.Prelude
-- import           Data.Map           (Map)
-- import qualified Data.Map           as M
-- import           Data.Set           (Set)
-- import qualified Data.Set           as S
import           Data.Aeson(ToJSON)
-- import           Data.Singletons.TH
-- import           GHC.TypeLits
import Data.Bifunctor.TH
type CVCLang = Text

-- type Trm = (Int, [CVCLang])


data SetOp = Union | DisjointUnion | Intersection | Difference deriving (Show, Generic, ToJSON)
data CompOp = Eq | LT | LTEq | GT | GTEq deriving (Show, Generic, ToJSON)


data SetLang e s = 
    SVar s
  | FSet [e]
  | SOp SetOp (Either s [SetLang e s]) -- Left means a list variable
  deriving (Show, Generic, ToJSON)

$(deriveBifunctor ''SetLang)

data IntLang e s = IntVal Int | Card (SetLang e s) deriving (Show, Generic, ToJSON)

$(deriveBifunctor ''IntLang)


data FormOp = And | Or | Impl deriving (Show, Generic, ToJSON)

data FormulaLang e s = 
    Member e (SetLang e s) -- 2 : {1,2,3}
  | BinSet CompOp (SetLang e s) (SetLang e s) -- {} <= {1}
  | BinInt CompOp (IntLang e s) (IntLang e s) -- | {} | >= 0
  | BinForm FormOp (FormulaLang e s) (FormulaLang e s) -- 2 : {1,2,3} -> {} = {}
  | Neg (FormulaLang e s) -- ~ ({} <= {1})
  deriving (Show, Generic, ToJSON)

$(deriveBifunctor ''FormulaLang)


-- run removeDisjoint before printAssert, as DisjointUnion is not a basic operation
removeDisjoint :: FormulaLang e s -> FormulaLang e s
removeDisjoint (BinSet Eq (SOp DisjointUnion xs) (SVar s)) = removeDisjoint (BinSet Eq (SVar s) (SOp DisjointUnion xs))
removeDisjoint (BinSet Eq (SVar s) (SOp DisjointUnion (Right xs))) = 
    foldr 
        (BinForm And) 
        (BinSet Eq (SVar s) (SOp Union (Right xs)))
        (map (\ys -> BinSet Eq (SOp Intersection (Right ys)) (FSet [])) $ pairs' xs)
    where
        pairs' l = [[x,y] | (x:ys) <- tails l, y <- ys]
removeDisjoint (BinForm o x y) = BinForm o (removeDisjoint x) (removeDisjoint y)
removeDisjoint (Neg x) = Neg $ removeDisjoint x
removeDisjoint x = x


-- printSet :: (Show e , Show s) => SetLang e s -> Text
-- printSet (SVar n) = show n
-- printSet (FSet []) = "({} :: SET OF INT)"
-- printSet (FSet ns) = "{" <> (T.intercalate "," $ map show ns) <> "}"
-- printSet (SOp Union (Right xs)) = T.intercalate " | " $ map printSet xs
-- -- printSet (SOp DisjointUnion a b) = printSet a <> " | " <> printSet b
-- printSet (SOp Intersection (Right xs)) = T.intercalate " & " $ map printSet xs
-- printSet (SOp Difference (Right xs)) = T.intercalate " - " $ map printSet xs

-- printInt :: (Show e , Show s) => IntLang e s -> Text
-- printInt (IntVal n) = show n
-- printInt (Card s) = "CARD(" <> show s <> ")"



-- printCompOp :: CompOp -> Text
-- printCompOp CTTerms.Core.Eq = "="
-- printCompOp CTTerms.Core.LT = "<"
-- printCompOp LTEq = "<="
-- printCompOp CTTerms.Core.GT = ">"
-- printCompOp GTEq = ">="


-- printAssert :: (Show e , Show s) => FormulaLang e s -> CVCLang
-- -- printAssert (SetDecl x) = (show x) <> " : SET OF INT;"
-- printAssert (AssertMember e x) = "ASSERT " <> (show e) <> " IS_IN " <> (printSet x) <> ";"
-- printAssert (AssertSet op x y) = "ASSERT " <> (printSet x) <> " " <> printCompOp op <> " " <> (printSet y) <> ";"
-- printAssert (AssertInt op x y) = "ASSERT " <> (printInt x) <> " " <> printCompOp op <> " " <> (printInt y) <> ";"
    

-- instance Bifunctor SetLang where
--     bimap _ g (SVar s) = SVar $ g s
--     bimap f _ (FSet ns) = FSet $ map f ns
--     bimap _ g (SOp o (Left s)) = SOp o (Left $ g s)
--     bimap f g (SOp o (Right xs)) = SOp o (Right $ map (bimap f g) xs)


-- instance Bifunctor IntLang where
--     bimap _ _ (IntVal i) = IntVal i
--     bimap f g (Card s) = Card $ bimap f g s

-- instance Bifunctor FormulaLang where
--     bimap f g (Member e s) = Member (f e) (bimap f g s)
--     bimap f g (BinSet op s1 s2) = BinSet op (bimap f g s1) (bimap f g s2)
--     bimap f g (BinInt op i1 i2) = BinInt op (bimap f g i1) (bimap f g i2)
--     bimap f g (BinForm op f1 f2) = BinForm op (bimap f g f1) (bimap f g f2)
--     bimap f g (Neg f1) = Neg (bimap f g f1)


empty :: SetLang e s
empty = FSet []

mkSet :: [e] -> SetLang e s
mkSet xs = FSet xs

-- union :: SetLang e s -> SetLang e s -> SetLang e s
-- union x y = Op Union x y

-- intersection :: SetLang e s -> SetLang e s -> SetLang e s
-- intersection x y = Op Intersection [x,y]




data Level = Term | Formula | Structure | Sequent deriving (Show, Generic, ToJSON)

deriving instance Eq Level
deriving instance Ord Level

type family Lower (l :: Level) = result | result -> l where
    Lower 'Structure = 'Formula
    Lower 'Formula = 'Term
    -- Lower 'Term = 'Name

type family Raise (l :: Level) = result | result -> l where
    Raise 'Formula = 'Structure
    Raise 'Term = 'Formula
    -- Raise 'Name = 'Term

-- type family IsName (l :: Level) where
--     IsName 'Name = 'True
--     IsName a = 'False




data CTyp e s = Any | FSetDecl [e] | CSetDecl s [FormulaLang e s] deriving (Show, Generic, ToJSON)
-- type CTyp t = (t, [FormulaLang t t]) 

$(deriveBifunctor ''CTyp)


data Type e sl = NVar (Maybe e) -- can only be an elem name
              | CType Level (CTyp e sl) -- can only be a set/list name
              | CListType Level (Maybe sl) deriving (Show, Generic, ToJSON)

$(deriveBifunctor ''CTTerms.Core.Type)



getLevel :: CTTerms.Core.Type t v -> Maybe Level
getLevel (NVar _) = Nothing
getLevel (CType l _) = Just l
getLevel (CListType l _) = Just l


-- instance Bifunctor CTTerms.Core.Type where
--     bimap _ vf (NVar v) = NVar $ map vf v
--     bimap tf _ (CType l t) = CType l $ map tf t
--     bimap _ vf (CListType l v) = CListType l $ liftM vf v

-- data Decl n a = Decl n [CTTerms.Core.Type (a, [FormulaLang a a]) a] (CTTerms.Core.Type (a, [FormulaLang a a]) a) 
--     deriving Show

-- instance Bifunctor Decl where
--     bimap nf af (Decl n ts t) = Decl (nf n) (map (bimap (bimap af (map (bimap af af))) af) ts) (bimap (bimap af (map (bimap af af))) af t)
-- -- data Assoc = Left | Right



-- data ParserSyntax = 
--     Const Text
--   | Ident
--   | IdentWithPrefix Text
--   | Prefix {
--         syntax :: Text,
--         fixity :: Int
--     }
--   | Infix {
--         syntax :: Text, 
--         fixity :: Int,
--         assoc :: Text.Earley.Mixfix.Associativity
--     }
--   | Mixfix {
--         syntax :: Text, 
--         fixity :: Int
--     }

-- data DeclSyntax a = DeclSyntax {
--     name :: a,
--     parserSyntax :: ParserSyntax,
--     katexSyntax :: Maybe Text, 
--     latexSyntax :: Text
-- }




data TermKind = Concrete | Meta

data CTTerm (l :: Level) (k :: TermKind) t a where
    Nm :: a -> CTTerm l k t a
    -- MVar :: SingI l => a -> t -> CTTerm l 'Meta t a
    Lift :: CTTerm (Lower l) k t a -> CTTerm l k t a
    Con :: -- SingI l => -- this is a bit of a hack :/
        Text -> [CTTerm l k t a] -> t -> CTTerm l k t a
    -- List :: [CTTerm 'Term k t a] -> CTTerm 'Term k t a
    -- Abbrev :: Text -> CTTerm (Raise l) 'Concrete t a -> CTTerm (Raise l) 'Concrete t a
    Seq :: CTTerm 'Structure k t a -> CTTerm 'Structure k t a -> CTTerm 'Sequent k t a

deriving instance (Show t , Show a) => Show (CTTerm l k t a)

$(deriveBifunctor ''CTTerm)

-- instance Bifunctor (CTTerm l k) where
--     bimap _ af (Nm a) = Nm $ af a
--     bimap tf af (Lift x) = Lift $ bimap tf af x
--     bimap tf af (Con c xs t) = Con c (map (bimap tf af) xs) (tf t)
--     bimap tf af (Seq l r) = Seq (bimap tf af l) (bimap tf af r)
