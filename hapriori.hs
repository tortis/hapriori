module Apriori where

import Data.List
import qualified Data.Set as S

type Sets a = [S.Set a]
type Rules a = [(S.Set a, Int)]

load_file :: String -> IO [S.Set String]
load_file filename = do
	io_dat <- readFile filename
	return $ map (S.fromList . words) (lines io_dat)

generate_c1 :: Ord a => Sets a -> Sets a
generate_c1 db = S.toList $ S.map S.singleton (S.unions db)

rmdups :: Ord a => Sets a -> Sets a
rmdups = map head . group .sort

generate_candidates :: Ord a => Sets a -> Sets a
generate_candidates l = rmdups [S.union a b | a<-l, b<-l, b < a, S.size (S.difference a b) == 1]

subsets :: Ord a => S.Set a -> Sets a
subsets s = [ S.fromList [ e | e<-sl, e /= i] | i<-sl]
    where sl = S.toList s

has_frequent_subs :: Ord a => S.Set a -> Sets a -> Bool
has_frequent_subs s low = and [ False | is<-(subsets s), is `notElem` low]

prune_candidates :: Ord a => Sets a -> Sets a -> Sets a
prune_candidates candidates low = [ c | c<-candidates, has_frequent_subs c low]

support_for :: Ord a => S.Set a -> Sets a -> Int
support_for set db = sum [ 1 | t <- db, set `S.isSubsetOf` t]

drop_candidates :: Ord a => Sets a -> Sets a -> Int -> Rules a
drop_candidates candidates db t = [ (c, sup) | c<-candidates, let sup = support_for c db, sup >= t]

higher_rules :: Ord a => Rules a -> Sets a -> Int ->  Rules a
higher_rules l db t = let ls = [ fst s | s<-l]
	in drop_candidates (prune_candidates (generate_candidates ls) ls) db t

apriori :: Ord a => Sets a -> Float -> [Rules a]
apriori db t = apriori' db l1 tn
    where   tn = truncate $ fromIntegral (length db) * t
            l1 = drop_candidates (generate_c1 db) db tn

apriori' :: Ord a => Sets a -> Rules a -> Int -> [Rules a]
apriori' db l t
	| null l = []
	| otherwise = ln:(apriori' db ln t)
	where ln = higher_rules l db t

