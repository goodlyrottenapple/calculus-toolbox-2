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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module CTTerms.Core where

-- import qualified Data.Text          as T
import           Lib.Prelude hiding (Type, Fixity, Associativity, Prefix, Infix)
-- import           Data.Map           (Map)
-- import qualified Data.Map           as M
-- import           Data.Set           (Set)
-- import qualified Data.Set           as S
import           Data.Aeson(ToJSON)
import           Data.Singletons.TH(genSingletons)
-- import           GHC.TypeLits
import Data.Bifunctor.TH
import           Text.Earley.Mixfix(Associativity(..))

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
    


empty :: SetLang e s
empty = FSet []

mkSet :: [e] -> SetLang e s
mkSet xs = FSet xs

-- union :: SetLang e s -> SetLang e s -> SetLang e s
-- union x y = Op Union x y

-- intersection :: SetLang e s -> SetLang e s -> SetLang e s
-- intersection x y = Op Intersection [x,y]




data Level = Term | Formula | Structure | Sequent deriving (Show, Generic, ToJSON)

genSingletons [''Level]


deriving instance Eq Level
deriving instance Ord Level

type family Lower (l :: Level) = result | result -> l where
  Lower 'Sequent = 'Structure
  Lower 'Structure = 'Formula
  Lower 'Formula = 'Term
  --Lower 'Term = 'Name

type family Raise (l :: Level) = result | result -> l where
  Raise 'Term = 'Formula
  Raise 'Formula = 'Structure
  Raise 'Structure = 'Sequent
  -- Raise 'Name = 'Term


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


data TermKind = Concrete | Meta deriving (Show, Eq)

genSingletons [''TermKind]


data CTTerm (l :: Level) (k :: TermKind) t a where
    Nm :: a -> CTTerm l k t a
    -- MVar :: SingI l => a -> t -> CTTerm l 'Meta t a
    Lift :: CTTerm (Lower l) k t a -> CTTerm l k t a
    Con :: -- SingI l => -- this is a bit of a hack :/
        Text -> [CTTerm l k t a] -> t -> CTTerm l k t a
    -- List :: [CTTerm 'Term k t a] -> CTTerm 'Term k t a
    Abbrev :: Text -> Maybe (CTTerm l 'Concrete t a) -> CTTerm l 'Concrete t a

deriving instance (Show t , Show a) => Show (CTTerm l k t a)
deriving instance (Eq t, Eq a) => Eq (CTTerm l k t a)
deriving instance (Ord t, Ord a) => Ord (CTTerm l k t a)


$(deriveBifunctor ''CTTerm)


data LevelList (l :: Level) a where
    One :: a -> LevelList 'Term a
    Cons :: a -> LevelList (Lower l) a -> LevelList l a 

deriving instance Show a => Show (LevelList l a)
deriving instance Functor (LevelList l)
deriving instance Foldable (LevelList l)


headLL :: LevelList l a -> a
headLL (One a) = a
headLL (Cons a _) = a


type family IsTerm (l :: Level) where
    IsTerm 'Term = 'True
    IsTerm l = 'False

tailLL :: IsTerm l ~ 'False => LevelList l a -> LevelList (Lower l) a
tailLL (Cons _ xs) = xs



-- isTerm :: LevelList l a -> Bool
-- isTerm (One _) = True
-- isTerm _ = False


mkLevelList :: a -> a -> a -> a -> LevelList 'Sequent a
mkLevelList a b c d = Cons a $ Cons b $  Cons c $ One d

newtype Module = Module [Text] 
  deriving (Show, Generic, Typeable, Ord, Eq)
  deriving anyclass ToJSON


newtype Binding = Binding Int 
    deriving (Eq, Ord, Generic) 
    deriving newtype (Show, Num, Bounded)
    deriving anyclass ToJSON

data Fixity = 
    Prefix { binding :: Binding }
  | Infix {
        binding :: Binding
      , assoc :: Associativity
    }
  | Mixfix { binding :: Binding } deriving (Show, Generic, ToJSON)

associativity :: Fixity -> Associativity
associativity (Prefix _) = RightAssoc
associativity (Infix _ a) = a
associativity (Mixfix _) = NonAssoc

data ConnDescription a t = ConnDescription {
    name    :: a
  , inTypes :: [t]
  , outType :: t
  , originalModule :: Module
  , as :: Maybe Text
  , fixity  :: Fixity
  , latex   :: Maybe a
  , katex   :: Maybe a
} deriving Show

$(deriveBifunctor ''ConnDescription)



data CalcDesc a r = CalcDesc {
    conns  :: LevelList 'Structure [ConnDescription a (Type a a)]
  , mod    :: Module
  , rules  :: r
  -- , macros :: Map Text (Int,Text)
} deriving Show


