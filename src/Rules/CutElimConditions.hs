{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE GADTs                 #-}

module Rules.CutElimConditions(conditionC1) where

import           Lib.Prelude
import           Terms
-- import Data.Set (Set)
import qualified Data.Set            as S

formulasInDSequent :: Ord (Term 'FormulaL k a) => DSequent k a -> Set (Term 'FormulaL k a)
formulasInDSequent (DSeq l _ r) = (formulasInDSequent' l) `S.union` (formulasInDSequent' r)
    where
        formulasInDSequent' :: Ord (Term 'FormulaL k a) => Term 'StructureL k a -> Set (Term 'FormulaL k a)
        formulasInDSequent' (Lift x) = S.singleton x
        formulasInDSequent' (Con _ xs) = foldr (S.union . formulasInDSequent') S.empty xs
        formulasInDSequent' (Meta _) = S.empty
        formulasInDSequent' (Abbrev _ x) = formulasInDSequent' x


subFormulas :: Ord (Term 'FormulaL k a) => Term 'FormulaL k a -> Set (Term 'FormulaL k a)
subFormulas x@(Con _ xs) = S.insert x $ foldr (S.union . subFormulas) S.empty xs
subFormulas x = S.singleton x

-- Each formula which is a constituent of some premise of a rule ρ 
-- is a sub-formula of some formula in the conclusion of ρ.
conditionC1 :: Ord a => Rule a -> Bool
conditionC1 (Rule _ _ prms cncl) = foldr ((&&) . (`S.isSubsetOf` subFormsInConcl) . formulasInDSequent) True prms
    where
        subFormsInConcl = foldr (S.union . subFormulas) S.empty $ formulasInDSequent cncl
conditionC1 (RevRule _ _ prm cncl) = 
    (formulasInDSequent prm) `S.isSubsetOf` subFormsInConcl &&
    (formulasInDSequent cncl) `S.isSubsetOf` subFormsInPrem
    where
        subFormsInConcl = foldr (S.union . subFormulas) S.empty $ formulasInDSequent cncl
        subFormsInPrem  = foldr (S.union . subFormulas) S.empty $ formulasInDSequent prm
