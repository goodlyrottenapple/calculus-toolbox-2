{-|
Module      : Lib
Description : Lib's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module RulesInv where

import Lib.Prelude
import SequentCalc
import System.Random
-- import Data.Array.IO
-- import Control.Monad
import qualified Prelude as P

-- import Data.Set(Set)
-- import qualified Data.Set as S
import qualified Data.Text as T

import Data.Map(Map)
import qualified Data.Map as M


newtype SMap a = SMap { unMap :: Map a () } deriving Show

instance Foldable SMap where
    foldr f e (SMap m) = foldr f e (map fst $ M.toList m)

type SequentS a = Sequent SMap a

deriving instance Show (SequentS Int)

instance Eq a => Eq (Sequent SMap a) where
    (Sequent (SMap x1) (SMap y1)) == (Sequent (SMap x2) (SMap y2)) = x1 == x2 && y1 == y2

instance Ord a => Ord (Sequent SMap a) where
    compare (Sequent (SMap x1) (SMap y1)) (Sequent (SMap x2) (SMap y2)) = case compare x1 x2 of
        EQ -> compare y1 y2
        x -> x

singleton :: a -> SMap a
singleton a = SMap $ M.singleton a ()

insert :: Ord a => a -> SMap a -> SMap a
insert a (SMap m) = SMap $ M.insert a () m

delete :: Ord a => a -> SMap a -> SMap a
delete n (SMap m) = SMap $ M.delete n m


(!!) :: SMap a -> Int -> a
(SMap m) !! i = fst $ M.elemAt i m

union :: Ord a => SMap a -> SMap a -> SMap a
union (SMap m) (SMap n) = SMap $ m `M.union` n


axiom :: a -> SequentS a
axiom a = Sequent (singleton $ Atom a) (singleton $ Atom a)


andL1 :: Ord a => Formula a -> Formula a -> SequentS a -> SequentS a
andL1 a b (Sequent g d) = Sequent (insert (And a b) $ delete a g) d


andL2 :: Ord a => Formula a -> Formula a -> SequentS a -> SequentS a
andL2 a b (Sequent g d) = Sequent (insert (And b a) $ delete a g) d



andR :: Ord a => Formula a -> Formula a -> SequentS a -> SequentS a -> SequentS a
andR a b (Sequent g1 d1) (Sequent g2 d2) = Sequent (g1 `union` g2) (insert (And a b) $ delete a $ delete b $ d1 `union` d2)



-- orR1 :: Ord a => Formula a -> Int -> SequentS a -> SequentS a
-- orR1 b a_i (Sequent g d) = Sequent g (insert (And a b) d)
--     where
--         a = d ! a_i


-- orR2 :: Ord a => Formula a -> Int -> SequentS a -> SequentS a
-- orR2 a b_i (Sequent g d) = Sequent g (insert (And a b) d)
--     where
--         b = d ! b_i



orL :: Ord a => Formula a -> Formula a -> SequentS a -> SequentS a -> SequentS a
orL a b (Sequent g1 d1) (Sequent g2 d2) = Sequent (insert (Or a b) $ delete a $ delete b $ g1 `union` g2) (d1 `union` d2)



impL :: Ord a => Formula a -> Formula a -> SequentS a -> SequentS a -> SequentS a
impL a b (Sequent g1 d1) (Sequent g2 d2) = Sequent (insert (Impl a b) $ delete b $ g1 `union` g2) (delete a $ d1 `union` d2)



-- impR :: Ord a => Int -> Int -> SequentS a -> SequentS a
-- impR a_i b_i (Sequent g d) = Sequent g (insert (Impl a b) d)
--     where
--         a = g ! a_i
--         b = d ! b_i



-- negL :: SequentS a -> SequentS a
-- negL (Sequent gamma (a:delta)) = Sequent ((Neg a):gamma) (a:delta)

-- negR :: SequentS a -> SequentS a
-- negR (Sequent (a:gamma) delta) = Sequent (a:gamma) ((Neg a):delta)


-- wL :: Formula a -> SequentS a -> SequentS a
-- wL a (Sequent gamma delta) = Sequent (a:gamma) delta

-- wR :: Formula a -> SequentS a -> SequentS a
-- wR a (Sequent gamma delta) = Sequent gamma (a:delta)


-- shuffle :: [a] -> IO [a]
-- shuffle xs = do
--         ar <- newArray n xs
--         forM [1..n] $ \i -> do
--             j <- randomRIO (i,n)
--             vi <- readArray ar i
--             vj <- readArray ar j
--             writeArray ar j vi
--             return vj
--   where
--     n = length xs
--     newArray :: Int -> [a] -> IO (IOArray Int a)
--     newArray n xs = newListArray (1,n) xs



-- -- shuffleL :: ([Formula a] -> [Formula a]) -> SequentS a -> SequentS a
-- -- shuffleL shuffle (SequentS a b) = Sequent (shuffle a) b

-- -- shuffleR :: ([Formula a] -> [Formula a]) -> SequentS a -> SequentS a
-- -- shuffleR shuffle (SequentS a b) = SequentS a (shuffle b)



-- fsRules :: Ord a => [Formula a -> Int -> SequentS a -> SequentS a]
-- fsRules = [andL1, andL2, orR1, orR2]


-- ssRules :: Ord a => [Int -> SequentS a -> Int -> SequentS a -> SequentS a]
-- ssRules = [orL, impL, andR]


-- sRules :: [SequentS a -> SequentS a]
-- sRules = [negL, negR, impR]

data RuleTypes = AndL1 | AndL2 | AndR | OrL | ImpL
    -- -- FSRule
    --            SSRule 
    --            -- | SRule
    --            | ShuffleL | ShuffleR
    deriving (Show, Bounded, Enum)




randomT :: (Bounded a, Enum a) => IO a
randomT = map (xs P.!!) $ randomRIO (0, length xs - 1) 
    where xs = [minBound ..]


pickS :: Show a => SMap a -> IO (Maybe a)
pickS s@(SMap m) = do
    i <- randomRIO (0, M.size m - 1)
    -- print $ ("picking " <> show i :: Text)
    if M.size m == 0 then return Nothing else return $ Just $ s !! i
 

seed :: Int -> SMap (SequentS Int)
seed n = SMap $ M.fromList $ map ((,()). axiom) [0..n] 


generateValidSequents :: Int -> SMap (SequentS Int) -> IO (SMap (SequentS Int))
generateValidSequents 0 xs = return xs
generateValidSequents iters xs = do
    t <- randomT :: IO RuleTypes
    case t of
        AndL1 -> do
            (Just s@(Sequent g1 _)) <- pickS xs
            (Just (Sequent g2 d)) <- pickS xs
            -- print "s1:"
            -- print $ pprintSeq s
            -- print "s2:"
            -- print $ pprintSeq $ Sequent g2 d
            a <- pickS g1
            -- toss <- randomT :: IO Bool
            b <- pickS g2
            -- print a
            case (a,b) of
                (Just a', Just b') -> generateValidSequents (iters-1) (insert (andL1 a' b' s) xs)
                _ -> generateValidSequents iters xs
        -- AndL2 -> do
        --     s@(Sequent g1 _) <- pickS xs
        --     (Sequent g2 d) <- pickS xs
        --     a <- pickS g1
        --     -- toss <- randomT :: IO Bool
        --     b <- pickS g2
        --     generateValidSequents (iters-1) (insert (andL2 a b s) xs)
        AndR -> do
            (Just s1@(Sequent _ d1)) <- pickS xs
            (Just s2@(Sequent _ d2)) <- pickS xs
            a <- pickS d1
            b <- pickS d2
            case (a,b) of
                (Just a', Just b') -> generateValidSequents (iters-1) (insert (andR a' b' s1 s2) xs)
                _ -> generateValidSequents iters xs
        OrL -> do
            (Just s1@(Sequent g1 _)) <- pickS xs
            (Just s2@(Sequent g2 _)) <- pickS xs
            a <- pickS g1
            b <- pickS g2
            case (a,b) of
                (Just a', Just b') -> generateValidSequents (iters-1) (insert (orL a' b' s1 s2) xs)
                _ -> generateValidSequents iters xs
        ImpL -> do
            (Just s1@(Sequent _ d1)) <- pickS xs
            (Just s2@(Sequent g2 _)) <- pickS xs
            a <- pickS d1
            b <- pickS g2
            case (a,b) of
                (Just a', Just b') ->  generateValidSequents (iters-1) (insert (impL a' b' s1 s2) xs)
                _ -> generateValidSequents iters xs

    --     -- SRule -> do
    --     --     rule <- pick sRules
    --     --     sequent <- pick xs
    --     --     generateValidSequents (iters-1) (rule sequent: xs)
    --     -- ShuffleL -> do
    --     --     (Sequent ls rs) <- pick xs
    --     --     rs' <- shuffle rs
    --     --     let sequent = Sequent ls rs'
    --     --         xs' = filter (/=(Sequent ls rs)) xs
    --     --     generateValidSequents (iters-1) (sequent:xs')
        _ -> generateValidSequents (iters-1) xs

filterTrivial :: [SequentS Int] -> [SequentS Int]
filterTrivial [] = []
filterTrivial (z@(Sequent (SMap xs) (SMap ys)):zs) = 
        if (M.size $ M.intersection xs ys) == 0 then z:(filterTrivial zs)
        else filterTrivial zs


pprint' :: Show a => Formula a -> Text
pprint' (Atom a) = toS $ (show a :: P.String)
pprint' (And a b) = "(" <> pprint' a <> "∧" <> pprint' b <> ")"
pprint' (Or a b) = "(" <> pprint' a <> "⋁" <> pprint' b <> ")"
pprint' (Impl a b) = "(" <> pprint' a <> "→" <> pprint' b <> ")"
pprint' (Neg a) = "¬" <> pprint' a


pprintSeq :: (Foldable t , Show a) => Sequent t a -> Text
pprintSeq (Sequent xs ys) = (T.intercalate ", " $ map pprint' $ toList xs) <> "⊢" <> (T.intercalate ", " $ map pprint' $ toList ys)



test :: Int -> Int -> IO ()
test iters n = do
    r <- generateValidSequents iters (seed n)
    writeFile "out.txt" $ T.intercalate "\n" $ 
        map pprintSeq $ filterTrivial $ toList r

-- -- permute :: Int -> [a] -> Maybe [a]
-- -- permute 0 xs = Just xs
-- -- permute n xs | n > 0 && n <= length xs = case uncons $ drop n xs of
-- --     Just (a, as) -> Just $ a : take n xs ++ as
-- --     Nothing -> Nothing -- unreachable
-- -- permute _ _ = Nothing


-- -- focusL :: Int -> Rule a
-- -- focusL n = Rule focusL'
-- --     where
-- --         focusL' (Sequent gamma delta) = case permute n gamma of
-- --             Just gamma' -> Just [Sequent gamma' delta]
-- --             Nothing -> Nothing

-- -- focusR :: Int -> Rule a
-- -- focusR n = Rule focusR'
-- --     where
-- --         focusR' (Sequent gamma delta) = case permute n delta of
-- --             Just delta' -> Just [Sequent gamma delta']
-- --             Nothing -> Nothing


