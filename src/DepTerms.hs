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

module DepTerms where

import qualified Data.Text          as T
import           Lib.Prelude
import           Data.Map           (Map)
import qualified Data.Map           as M
import           Data.Set           (Set)
import qualified Data.Set           as S
import           Data.Aeson
import           Data.Singletons.TH
import           GHC.TypeLits

-- import           Data.Singletons.TH
-- import           Data.Aeson
-- import           Text.Earley.Mixfix (Associativity)
-- import           GHC.TypeLits


-- data TermKind = Star | StarSet | ConcreteSet (Set Text) | Wildcard



-- data ConstraintTType n = ElemOf n
-- data TermType = ConcreteT Text -- (is of kind Star)
--               | Var Int TermKind
--               | ConceteSetT (Set Text) -- (is of kind StarSet)
--               | FunT Text [TermType] TermKind



-- commaT :: Text -> Text -> TermType
-- commaT y ys = FunT "comma" [Var y Star, Var ys StarSet] StarSet



--     -- ConstraintT :: ConstraintTType name -> TermType name -> TermType name


-- data DCon = DCon {
--     name :: Text,
--     inTypes :: [TermType],
--     outType :: TermType
-- }


-- circ :: DCon
-- circ = DCon "circ" [Var "ys" StarSet] (commaT "y" "ys")

-- ex :: DCon
-- ex = DCon "ex" [commaT "y" "ys"] (Var "ys" StarSet)


-- and :: DCon
-- and = DCon "circ" [Var "ys" StarSet, Var "ys" StarSet] (Var "ys" StarSet)


-- triangle :: DCon
-- triangle = DCon "triangle" [Var "y" (ConcreteSet (S.fromList ["Ag", "Act", "Fnc"])), ConcreteT "Fm"] (ConcreteT "Fm")



-- widlcardKindVar :: Text -> TermType
-- widlcardKindVar x = Var x Wildcard

-- data DTerm = Base Text TermType
--            | Lift DTerm
--            | Conn DCon [DTerm]

-- mkBase :: Text -> Int -> DTerm
-- mkBase n t = Base n (Var i Star)


-- mkConn :: DCon -> [DTerm] -> DTerm
-- mkConn c ts = undefined -- unification here??

-- type UnficationMap = Map (Set Text) TermType

type AssertSet = Text

type Trm = (Int, [AssertSet])


data SetOp = Empty | MkSet | Union | Intersection | Difference deriving Show

data SetCompOp = SEq | SubsetEq deriving Show
data IntCompOp = IEq | LT | LTEq | GT | GTEq deriving Show

data SetLang e s = SetName s
             | ElemName e
             | Op SetOp [SetLang e s] deriving (Show)

data IntLang s = IntVal Int | Card s deriving (Show, Functor)


data AssertSetLang e s = SetDecl s -- t : SET OF INT
                | AssertMember e (SetLang e s) -- AssertSet e IS_IN s
                | AssertSet SetCompOp (SetLang e s) (SetLang e s) -- AssertSet ?s1 op ?s2 ????
                | AssertInt IntCompOp (IntLang s) (IntLang s) -- AssertSet ?s1 op ?s2 ????
                  deriving (Show)


printSet :: (Show e , Show s) => SetLang e s -> Text
printSet (SetName n) = show n
printSet (ElemName n) = show n
printSet (Op Empty []) = "({} :: SET OF INT)"
printSet (Op MkSet xs) | all isElemName xs = "{" <> (T.intercalate "," $ map printSet xs) <> "}"
printSet (Op Union [a,b]) = printSet a <> " | " <> printSet b
printSet (Op Intersection [a,b]) = printSet a <> " & " <> printSet b
printSet (Op Difference [a,b]) = printSet a <> " - " <> printSet b
printSet (Op _ _) = undefined

isElemName :: SetLang e s -> Bool
isElemName (ElemName _) = True
isElemName _ = False


printInt :: Show s => IntLang s -> Text
printInt (IntVal n) = show n
printInt (Card s) = "CARD(" <> show s <> ")"



printAssert :: (Show e , Show s) => AssertSetLang e s -> AssertSet
printAssert (SetDecl x)    = (show x) <> " : SET OF INT;"
printAssert (AssertMember e x) = "ASSERT " <> (show e) <> " IS_IN " <> (printSet x) <> ";"
printAssert (AssertSet SEq x y) = "ASSERT " <> (printSet x) <> " = " <> (printSet y) <> ";"
printAssert (AssertSet SubsetEq x y) = "ASSERT " <> (printSet x) <> " SUBSET_EQ?? " <> (printSet y) <> ";"
printAssert (AssertInt op x y) = "ASSERT " <> (printInt x) <> " " <> printIntCompOp op <> " " <> (printInt y) <> ";"
    where
        printIntCompOp :: IntCompOp -> Text
        printIntCompOp IEq = "="
        printIntCompOp DepTerms.LT = "<"
        printIntCompOp LTEq = "<="
        printIntCompOp DepTerms.GT = ">"
        printIntCompOp GTEq = ">="

instance Bifunctor SetLang where
    bimap _ g (SetName s) = SetName $ g s
    bimap f _ (ElemName e) = ElemName $ f e
    bimap f g (Op o xs) = Op o $ map (bimap f g) xs

instance Bifunctor AssertSetLang where
    bimap _ g (SetDecl s) = SetDecl $ g s
    bimap f g (AssertMember e s) = AssertMember (f e) (bimap f g s)
    bimap f g (AssertSet op s1 s2) = AssertSet op (bimap f g s1) (bimap f g s2)
    bimap f g (AssertInt op i1 i2) = AssertInt op (map g i1) (map g i2)



empty :: SetLang e s
empty = Op Empty []

mkSet :: [e] -> SetLang e s
mkSet [] = DepTerms.empty
mkSet xs = Op MkSet $ map ElemName xs

union :: SetLang e s -> SetLang e s -> SetLang e s
union x y = Op Union [x,y]

intersection :: SetLang e s -> SetLang e s -> SetLang e s
intersection x y = Op Intersection [x,y]




$(singletons [d| data Level = Name | Term | Formula | Structure deriving (Show, Generic, ToJSON, FromJSON) |])

deriving instance Eq Level
deriving instance Ord Level

type family Lower (l :: Level) = result | result -> l where
    Lower 'Structure = 'Formula
    Lower 'Formula = 'Term
    Lower 'Term = 'Name

type family Raise (l :: Level) = result | result -> l where
    Raise 'Formula = 'Structure
    Raise 'Term = 'Formula
    Raise 'Name = 'Term

type family IsName (l :: Level) where
    IsName 'Name = 'True
    IsName a = 'False



type CTyp t = t -> Map t t -> [AssertSetLang t t]

data Type t = NVar (Maybe t) -- can only be a name?
            | NList
            | CType Level (CTyp t)
            | CListType Level (CTyp t)



data Decl t = Decl Text [DepTerms.Type t] (DepTerms.Type t)



-- a special kind of map lookup, where, if a map doesnt contain the given key, the key is returned
-- we can use this to handle constants vs variables.... i.e. if we write {Fm, Ag, Act, Fnc} and 
-- Fm, Ag, ... etc are not in the map, they are treated as constants...

(?!) :: Ord k => Map k k -> k -> k
m ?! k = case M.lookup k m of
    Just v -> v
    Nothing -> k



-- var : (x : Name) -> Atom (t where t = {x}) 

varDecl :: Decl Text
varDecl = Decl "var" [NVar (Just "x")] 
    (CType Term (\t dMap -> [SetDecl t , AssertSet SEq (SetName t) (mkSet [ dMap M.! "x" ])]))


-- var2 : (x : Name) -> Atom (t where x ∈ t)

var2Decl :: Decl Text
var2Decl = Decl "var2" [NVar (Just "x")]
    (CType Term (\t dMap -> [SetDecl t , AssertMember (dMap M.! "x") (SetName t)]))


-- var3 : Name -> Atom (t where t ⊆ {Fm, Ag, Act, Fnc} ; card t = 1) 

var3Decl :: Decl Text
var3Decl = Decl  "var3" [NVar Nothing] 
    (CType Term (\t dMap -> [
        SetDecl t , 
        AssertSet SubsetEq (SetName t) (mkSet ["Fm" , "Ag", "Act", "Fnc"]),
        AssertInt IEq (Card "t") (IntVal 1) ]))



-- replaceS :: Eq s => s -> AssertSetLang e s -> s -> AssertSetLang e s
-- replaceS s assert s' = bimap identity (\x -> if x == s then s' else x) assert


-- replaceE :: Eq e => e -> AssertSetLang e s -> e -> AssertSetLang e s
-- replaceE e assert e' = bimap (\x -> if x == e then e' else x) identity assert


newtype Conn (l :: Level) (n :: Nat) = C Text deriving (Eq, Show)


data TermKind = Concrete | Meta

data CTTerm (l :: Level) (k :: TermKind) a where
    Nm :: a -> CTTerm 'Name 'Concrete a
    MVar :: SingI l => a -> CTTerm l 'Meta a
    Lift :: SingI l => CTTerm (Lower l) k a -> CTTerm l k a
    Con :: (KnownNat n, SingI l, IsName l ~ 'False) => -- this is a bit of a hack :/
        Conn l n -> Vec n (CTTerm l k a) -> CTTerm l k a
    Abbrev :: Text -> CTTerm (Raise l) 'Concrete a -> CTTerm (Raise l) 'Concrete a

deriving instance Show a => Show (CTTerm l k a)



-- -- data Typ = Simple Text | List [Typ] | Constraint Level [AssertSet]
-- -- data Args = UnNamed Text Typ | TermArg Term
-- data Term = Base Text Typ | Con Text [Term] Level [AssertSet]


-- var :: Text -> Term
-- var x = Base x (Constraint Term )

-- -- -- p :: Ident -> [Ident] -> Term t where t ...
-- -- p :: Text -> [Text] -> Term
-- -- p pName [xs] = Con pName (map )


-- dot :: Int -> Trm -> State Int Trm
-- dot x (t, as) = do
--     fresh <- get
--     put (fresh+2)
--     let as' = [
--             "t" <> (show fresh) <> " : SET OF INT;" , 
--             "t" <> (show $ fresh + 1) <> " : SET OF INT;",
--             "ASSERT t" <> (show fresh) <> " = t" <> (show t) <> ";",
--             "ASSERT {" <> (show x) <> "} & t" <> (show fresh) <> " = ({} :: SET OF INT);",
--             "ASSERT t" <> (show $ fresh + 1) <> " = {" <> (show x) <> "} | t" <> (show fresh) <> ";"] ++ as

--     return (fresh, as')



-- p :: [Int] -> State Int Trm
-- p xs = do
--     fresh <- get
--     put (fresh+1)
--     return (fresh , [
--         "t" <> (show fresh) <> " : SET OF INT;" , 
--         "ASSERT t" <> (show fresh) <> " = {" <> (T.intercalate "," $ map show xs) <> "};"])

