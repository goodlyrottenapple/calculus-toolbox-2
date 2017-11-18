{-|
Module      : Lib
Description : Lib's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module Rules where

import Lib.Prelude
import SequentCalc

id :: Eq a => Rule a
id = Rule id'
    where 
        id' (Sequent [Atom a] [Atom b]) | a == b = Just []
        id' _ = Nothing

cut :: Formula a -> Rule a
cut f = Rule cut'
    where
        cut' (Sequent gamma delta) = Just [Sequent gamma (f:delta), Sequent (f:gamma) delta]

andL1 :: Rule a
andL1 = Rule andL1'
    where
        andL1' (Sequent ((And a _):gamma) delta) = Just [Sequent (a:gamma) delta]
        andL1' _ = Nothing

andL2 :: Rule a
andL2 = Rule andL2'
    where
        andL2' (Sequent ((And _ b):gamma) delta) = Just [Sequent (b:gamma) delta]
        andL2' _ = Nothing

andR :: Rule a
andR = Rule andR'
    where
        andR' (Sequent gamma ((And a b):delta)) = Just [Sequent gamma (a:delta), Sequent gamma (b:delta)]
        andR' _ = Nothing


orR1 :: Rule a
orR1 = Rule orR1'
    where
        orR1' (Sequent gamma ((Or a _):delta)) = Just [Sequent gamma (a:delta)]
        orR1' _ = Nothing

orR2 :: Rule a
orR2 = Rule orR2'
    where
        orR2' (Sequent gamma ((Or _ b):delta)) = Just [Sequent gamma (b:delta)]
        orR2' _ = Nothing


orL :: Rule a
orL = Rule orL2'
    where
        orL2' (Sequent ((Or a b):gamma) delta) = Just [Sequent (a:gamma) delta, Sequent (b:gamma) delta]
        orL2' _ = Nothing

impL :: Rule a
impL = Rule impL'
    where
        impL' (Sequent ((Impl a b):gamma) delta) = Just [Sequent gamma (a:delta), Sequent (b:gamma) delta]
        impL' _ = Nothing

impR :: Rule a
impR = Rule impR'
    where
        impR' (Sequent gamma ((Impl a b):delta)) = Just [Sequent (a:gamma) (b:delta)]
        impR' _ = Nothing

negL :: Rule a
negL = Rule negL'
    where
        negL' (Sequent ((Neg a):gamma) delta) = Just [Sequent gamma (a:delta)]
        negL' _ = Nothing

negR :: Rule a
negR = Rule negL'
    where
        negL' (Sequent gamma ((Neg a):delta)) = Just [Sequent (a:gamma) delta]
        negL' _ = Nothing

wL :: Rule a
wL = Rule wL'
    where
        wL' (Sequent (_:gamma) delta) = Just [Sequent gamma delta]
        wL' _ = Nothing

wR :: Rule a
wR = Rule wR'
    where
        wR' (Sequent gamma (_:delta)) = Just [Sequent gamma delta]
        wR' _ = Nothing


permute :: Int -> [a] -> Maybe [a]
permute 0 xs = Just xs
permute n xs | n > 0 && n <= length xs = case uncons $ drop n xs of
    Just (a, as) -> Just $ a : take n xs ++ as
    Nothing -> Nothing -- unreachable
permute _ _ = Nothing


focusL :: Int -> Rule a
focusL n = Rule focusL'
    where
        focusL' (Sequent gamma delta) = case permute n gamma of
            Just gamma' -> Just [Sequent gamma' delta]
            Nothing -> Nothing

focusR :: Int -> Rule a
focusR n = Rule focusR'
    where
        focusR' (Sequent gamma delta) = case permute n delta of
            Just delta' -> Just [Sequent gamma delta']
            Nothing -> Nothing


