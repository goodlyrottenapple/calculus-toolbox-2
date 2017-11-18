{-|
Module      : Lib
Description : Lib's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module SequentCalc where

import Lib.Prelude
import qualified Prelude as P
-- import qualified Data.Text as T

import Data.Map(Map)
import qualified Data.Map as M


import Terms
import Terms.TH
import Terms.Parsers

import GHC.TypeLits
-- import Unsafe.Coerce(unsafeCoerce)
import Data.Singletons.TH
import Data.Tree
import Data.Tree.Pretty
import Data.Aeson
-- import Data.Data
-- import Text.Parsec (ParseError, parse, between, eof, try)
-- import Text.Parsec.Text (Parser)
-- import Text.Parsec.Char (oneOf, char, digit, string, letter, satisfy, spaces, endOfLine)


data Formula a = Atom a 
               | And (Formula a) (Formula a) 
               | Or (Formula a) (Formula a)
               | Impl (Formula a) (Formula a)
               | Neg (Formula a) deriving (Eq, Ord, Show)

data Sequent c a = Sequent (c (Formula a)) (c (Formula a))

deriving instance Show a => Show (Sequent [] a)

-- instance (Foldable c, Eq a) => Eq (Sequent c a) where



newtype Rule a = Rule { apply :: Sequent [] a -> Maybe [Sequent [] a] }


-- 

-- instance Unifiable Formula where
--     unify (Atom a) (Atom b) = Just $ S.singleton $ (a,b)
--     unify ()


-- newtype Fix f = Fix (f (Fix f))

-- type Meta (f :: * -> * -> *) a b = Fix ((f a) :+: b)



-- -- atoms


-- newtype AtF a b = AtF a
-- deriveBifunctor ''AtF
-- newtype AtFV a c = AtFV a

-- -- type MetaAt' a b = Fix (AtF a :+: AtFV b)

-- type MetaAt a b = SequentCalc.Meta AtF a (AtFV b)
-- type At a = Fix (AtF a)

-- pattern FAt a = Fix (AtF a)

-- --formulas
-- data FormulaF a b = AtomF a
--                | AndF b b
--                | OrF b b
--                | ImplF b b
--                | NegF b 
-- deriveBifunctor ''FormulaF
-- newtype FormulaFV a b = FormulaFV a

-- pattern FAtom a = Fix (AtomF a)
-- pattern FAnd a b = Fix (AndF a b)
-- pattern FOr a b = Fix (OrF a b)
-- pattern FImpl a b = Fix (OrF a b)




-- type MetaForm a b = SequentCalc.Meta FormulaF (MetaAt a b) (FormulaFV b)
--     -- Fix (FormulaF (MetaAt a b) :+: FormulaFV b)
-- type Form a = Fix (FormulaF (At a))




-- --structures
-- data StructureF f s = FormF f
--                | CommaF s s
--                | ArrF s s
--                | I
-- deriveBifunctor ''StructureF
-- newtype StructureFV a b = StructureFV a


-- type MetaStruct a b = SequentCalc.Meta StructureF (MetaForm a b) (StructureFV b)
-- type Struct a = Fix (StructureF (Form a))



-- class Unifiable s s' sm a b where
--     unify :: Fix (s' a :+: sm b) -> Fix (s a) -> Maybe (Set (a,b))
--     -- sub :: Set (a,b) -> t a -> t b



-- class ToM s a b where
--     toM :: Bifunctor s => Fix (s a) -> Fix (s b :+: m)

-- instance (Bifunctor f, ToM f a b) => ToM s (Fix (f a)) (Fix (f b :+: n)) where
--     toM (Fix s) = Fix (L1 (bimap toM toM s))


-- x :: IO [TH.Dec]
-- x= TH.runQ (deriveBifunctor ''StructureF) --[d| newtype AtF a b = AtF a |]







-- pattern A x = Lift (Base x)
-- pattern CommaLK x y = HomC CommaLKC (x :> y :> Nil)
-- pattern x :&: y = HomC AndLKC (x :> y :> Nil)
-- pattern x :|: y = HomC OrLKC (x :> y :> Nil)
-- pattern x :->: y = HomC ImpLKC (x :> y :> Nil)
-- pattern Not x = HomC NegLKC (x :> Nil)


-- data instance HomCon "LK" l n f p b ltx where
--     CommaLKC :: HomCon "LK" 'StructureL 2 'InfixL             ";"     1 '("comma" , "\\color{blue}{{#1}}")
--     AndLKC ::   HomCon "LK" 'FormulaL   2 'InfixL             "/\\"   1 '("and" , "\\color{blue}{{#1}}")
--     OrLKC ::    HomCon "LK" 'FormulaL   2 'InfixL             "\\/"   1 '("or" , "\\color{blue}{{#1}}")
--     ImpLKC ::   HomCon "LK" 'FormulaL   2 'InfixL             "->"    1 '("impl" , "\\color{blue}{{#1}}")
--     NegLKC ::   HomCon "LK" 'FormulaL   1 'Terms.Prefix        "~"    1 '("neg" , "\\color{blue}{{#1}}")
--     -- Box ::      HomCon "LK" 'FormulaL   2 'Terms.Prefix "[]"    1 '("comma" , "\\color{blue}{{#1}}")


-- deriving instance Eq (HomCon "LK" l n f p b ltx)
-- $(generateMkHomCon "LK")
-- $(generateParserData "LK")



-- type LKRule a = HomRule "LK" () a

-- andLKL1 :: LKRule Char
-- andLKL1 = SRule [ CommaLK (MetaV 'X') (Lift $ (MetaV 'A') :&: (MetaV 'B')) :|-: MetaV 'Z' ] 
--                 (CommaLK (MetaV 'X') (Lift $ MetaV 'A') :|-: MetaV 'Z')

-- currParser :: Parser (Term "LK" 'FormulaL 'True Text)
-- currParser = 
-- --(Text.Parsec.try $ parens $ parser) <|> 
--     (Text.Parsec.try pMetaV) <|> (Text.Parsec.try pNot) <|> (Text.Parsec.try pAnd)
    
--     -- <|> pLift
--     where
--         pMetaV = MetaV <$> toS <$> lexeme ((string "?F" *> identifier) <|> identifier)
--         -- pLift = Lift <$> parser
--         pAnd = (HomC AndLKC) <$> (
--             (:>) <$> (currParser <* string "/\\") <*> ((\x -> x :> Nil) <$> currParser)   
--              )

--         pNot = (\x -> HomC NegLKC $ x :> Nil) <$> (string "~" *> currParser)
--         -- pParens = 



-- instance TermParser "LK" 'FormulaL 'True Text where
--     parser = currParser
--         where
            


-- parseF :: Text -> Either ParseError (Term "LK" 'FormulaL 'True Text)
-- parseF s = parse ((lexeme parser) <* eof) "" s 


-- -- andLKL1 =  [rule| ?X |- ?A    ?X |- ?B
-- --                   --------------------
-- --                     ?X |- ?A /\ ?B |]


-- -- pattern Box a x = HetC BoxC (Base a) x

