{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module   : Zora.List
-- Copyright   : (c) Brett Wines 2014
--
-- License   : BSD-style
--
-- Maintainer  : bgwines@cs.stanford.edu
-- Stability   : experimental
-- Portability : portable
-- 
-- Assorted functions on lists.
--

module Zora.List
(
-- * Partitioning
  partition_with_block_size
, partition_into_k
, powerpartition

-- * List transformations
, uniqueify
, pairify
, decyclify
, shuffle

-- * Permutations, combinations, and cycles
, powerset
, permutations
, subsets_of_size
, subsets_of_size_with_replacement
, cycles
, has_cycles

-- * Operations with two lists
, diff_infinite
, merge
, merge_by
, zip_while

-- * Sublists
, remove_at_index
, subseq
, take_while_keep_last
, take_while_and_rest
, find_and_rest
, subsequences
, contiguous_subsequences
, contiguous_subsequences_of_length

-- * Sorting
, is_sorted
, mergesort

-- * Predicates
, is_palindrome
, contains_duplicates

-- * Assorted functions
, bsearch
, bsearch_1st_geq
, elem_counts
, elem_counts_by
, running_bests
, running_bests_by
, (<$*>)
, ($$)
, interleave
, passing_index_elems
, count
, map_keep
, maximum_with_index
, minimum_with_index
, maxima_by
, minima_by
, length'
, drop'
, take'
, cons
, snoc

-- * Tuples
, map_fst
, map_snd
, map_pair
, map_pair_same
, map_triple
, zip_with_pair
, zip_with_pair_same
, fst3
, snd3
, trd3
, pair_op
, triple_op
) where

import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Set as Set

import System.Random

import Debug.Trace

import Control.Applicative

import Data.Monoid
import Data.Maybe

-- ---------------------------------------------------------------------
-- Partitioning

-- | /O(n)/ Partitions the given list into blocks of the specified length. Truncation behaves as follows:
-- 
-- > partition_with_block_size 3 [1..10] == [[1,2,3],[4,5,6],[7,8,9],[10]]
partition_with_block_size :: Int -> [a] -> [[a]]
partition_with_block_size len l =
    if (length l) <= len
        then [l]
        else (take len l) : (partition_with_block_size len (drop len l))

-- | /O(n)/ Partitions the given list into /k/ blocks. Truncation behavior is best described by example:
-- 
-- > partition_into_k  3 [1..9]  == [[1,2,3],[4,5,6],[7,8,9]]
-- > partition_into_k  3 [1..10] == [[1,2,3,4],[5,6,7,8],[9,10]]
-- > partition_into_k  3 [1..11] == [[1,2,3,4],[5,6,7,8],[9,10,11]]
-- > partition_into_k  3 [1..12] == [[1,2,3,4],[5,6,7,8],[9,10,11,12]]
-- > partition_into_k  3 [1..13] == [[1,2,3,4,5],[6,7,8,9,10],[11,12,13]]
partition_into_k :: Int -> [a] -> [[a]]
partition_into_k k arr = partition_with_block_size block_size arr
    where
        block_size :: Int
        block_size = if (((length arr) `mod` k) == 0)
            then (length arr) `div` k
            else (length arr) `div` k + 1

-- | /O(B(n))/, where /B(n)/ is the /n/^th <http://en.wikipedia.org/wiki/Bell_number Bell number>. Computes all partitions of the given list. For example,
-- 
-- > powerpartition [1..3] == [[[1],[2],[3]], [[1,2],[3]], [[2],[1,3]], [[1],[2,3]], [[1,2,3]]]
powerpartition :: [a] -> [[[a]]]
powerpartition [] = []
powerpartition l@(x:xs) =
    if length l == 1
        then [[[x]]]
        else concatMap (get_next_partitions x) . powerpartition $ xs
        where
            get_next_partitions :: a -> [[a]] -> [[[a]]]
            get_next_partitions e l = ([e] : l) : (map f indices)
                where
                    f i = (a i) ++ (b i) ++ (c i)
                    
                    a i = ((take' i) l)
                    b i = [e : (l !! (fromInteger i))]
                    c i = (drop' (i+1) l)

                    indices = [0..((length' l) - 1)]

-- ---------------------------------------------------------------------
-- List transformations

-- | /O(n log(n))/ Removes duplicate elements. Like `Data.List.nub`, but for `Ord` types, so it can be faster.
uniqueify :: (Ord a) => [a] -> [a]
uniqueify = Set.toList . Set.fromList

-- | /O(n log(n))/ Removes duplicate elements according to the given comparator function. Like `Data.List.nub`, but for `Ord` types, so it can be faster.
uniqueify_by :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a]
uniqueify_by cmp
    = map head
    . List.groupBy (\x y -> EQ == (cmp x y))
    . List.sortBy cmp

-- | /O(n)/ Zips the list up into pairs. For example,
-- 
-- > pairify [1..6] == [(1,2), (3,4), (5,6)]
-- > pairify [1..5] == [(1,2), (3,4)]
pairify :: [a] -> [(a, a)]
pairify l@(a:b:l') = (a, b) : pairify l'
pairify l = []

-- | /O(l m)/, where /l/ is the cycle length and /m/ is the index of the start of the cycle. If the list contains no cycles, then the runtime is /O(n)/.
--
-- NOTE: this function will only find cycles in a list can be the output of an iterated function -- that is, no element may be succeeded by two separate elements (e.g. [2,3,2,4]).
decyclify :: (Eq a) => [a] -> [a]
decyclify = fromJust . List.find (not . has_cycles) . iterate decyclify_once

decyclify_once :: (Eq a) => [a] -> [a]
decyclify_once l =
    if isNothing lambda
        then l
        else take' (lambda' + mu'') l
    where
        i    = alg1 (tail l) (tail . tail $ l)
        i'  = fromJust i

        mu  = if (i  == Nothing) then Nothing else alg2 l (drop' i' l)
        mu'  = fromInteger . fromJust $ mu
        mu''    = fromJust mu

        lambda  = if (mu == Nothing) then Nothing else alg3 (drop (mu' + 1) l) (l !! mu')
        lambda' = fromJust lambda

        alg1' :: (Eq a) => Integer -> [a] -> [a] -> Maybe Integer
        alg1' i _ [] = Nothing
        alg1' i [] _ = Nothing
        alg1' i l1 l2 = 
            if (head l1) == (head l2) then
                Just (i + 1) -- + 1 beacuse we start off with l1 = tail l
            else
                if tail l2 == [] then
                    Nothing
                else
                    alg1' (i + 1) (tail l1) (tail . tail $ l2)

        alg2' :: (Eq a) => Integer -> [a] -> [a] -> Maybe Integer
        alg2' mu _  [] = Nothing
        alg2' mu [] _  = Nothing
        alg2' mu l1 l2 = 
            if (head l1) == (head l2) then
                Just mu
            else
                alg2' (mu + 1) (tail l1) (tail l2)

        alg3' :: (Eq a) => Integer -> [a] -> a -> Maybe Integer
        alg3' lambda [] e = Nothing
        alg3' lambda l e =
            if (head l) == e then
                Just lambda
            else
                alg3' (lambda + 1) (tail l) e

        alg1 :: (Eq a) => [a] -> [a] -> Maybe Integer
        alg1 = alg1' 0

        alg2 :: (Eq a) => [a] -> [a] -> Maybe Integer
        alg2 = alg2' 0

        alg3 :: (Eq a) => [a] -> a -> Maybe Integer
        alg3 = alg3' 1

shuffle' :: [a] -> [Integer] -> [a]
shuffle' l indices =
    map fst . List.sortBy (Ord.comparing snd) $ zip l indices

-- | /O(n log(n))/ Shuffles the given list. The second parameter is the seed for the random number generator that backs the shuffle.
shuffle :: forall a. (Eq a) => [a] -> Integer -> [a]
shuffle l seed = shuffle' l randomness
    where
        randomness :: [Integer]
        randomness = prep (length' l) $ random_integers (0, (length' l) - 1) seed

        random_integers :: (Integer, Integer) -> Integer -> [Integer]
        random_integers range = randomRs range . mkStdGen . fromInteger

        prep :: Integer -> [Integer] -> [Integer]
        prep len l' = reverse . take' len $ prep' l' []

        prep' :: [Integer] -> [Integer] -> [Integer]
        prep' [] seen = []
        prep' src seen =
            if head src `elem` seen
                then             prep' (tail src) seen
                else head src : (prep' (tail src) (head src : seen))

-- ---------------------------------------------------------------------
-- Permutations, combinations, and cycles

-- | /O(2^n)/ Computes the powerset of the given list.
powerset :: [a] -> [[a]]
powerset l = powerset_rec l []
    where
        powerset_rec :: [a] -> [a] -> [[a]]
        powerset_rec [] so_far = [so_far]
        powerset_rec src so_far = without ++ with
            where without = powerset_rec (tail src) (so_far)
                  with  = powerset_rec (tail src) ((head src) : so_far)

-- TODO: actually O(n!)?
-- | /O(n!)/ Computes all permutations of the given list.
permutations :: [a] -> [[a]]
permutations l
    | (length l <= 1) = [l]
    | otherwise = 
        let splice_pairs = [(l !! i, remove_at_index (toInteger i) l) | i <- [0..((length l) - 1)]]
        in
            concat [
                [fst splice_pair : recpair | recpair <- permutations $ snd splice_pair]
                | splice_pair <- splice_pairs
            ]

-- | /O(2^k)/ Generates all subsets of the given list of size /k/.
subsets_of_size :: [a] -> Integer -> [[a]]
subsets_of_size l size = subsets_of_size_rec l [] size
    where
        subsets_of_size_rec :: [a] -> [a] -> Integer -> [[a]]
        subsets_of_size_rec src so_far size =
            if size == 0
                then [so_far]
                else if (length src) == 0
                    then []
                    else without ++ with
            where
                without = subsets_of_size_rec (tail src) so_far size
                with    = subsets_of_size_rec (tail src) ((head src) : so_far) (size-1)

-- | /O(n^m)/ Computes all sets comprised of elements in the given list, where the elements may be used multiple times, where `n` is the size of the given list and `m` is the size of the sets to generate. For example,
--
--   > subsets_of_size_with_replacement 3 [1,2] == [[1,1,1],[2,1,1],[1,2,1],[2,2,1],[1,1,2],[2,1,2],[1,2,2],[2,2,2]]

subsets_of_size_with_replacement :: Integer -> [a] -> [[a]]
subsets_of_size_with_replacement n src = subsets_of_size_with_replacement' n src []
    where
        subsets_of_size_with_replacement' :: forall a. Integer -> [a] -> [a] -> [[a]]
        subsets_of_size_with_replacement' 0 src so_far = [so_far]
        subsets_of_size_with_replacement' n src so_far
            = concatMap (subsets_of_size_with_replacement' (n-1) src) $ nexts
            where
                nexts :: [[a]]
                nexts = map (: so_far) src

-- | /O(n)/ Generates all cycles of a given list. For example,
-- 
-- > cycles [1..3] == [[2,3,1],[3,1,2],[1,2,3]]
cycles :: (Eq a) => [a] -> [[a]]
cycles l = cycles_rec l $ cycle_list l
    where
        cycle_list :: [a] -> [a]
        cycle_list l = (tail l) ++ [head l]

        cycles_rec :: (Eq a) => [a] -> [a] -> [[a]]
        cycles_rec original_l l
            | l == original_l = [l]
            | otherwise = [l] ++ (cycles_rec original_l $ cycle_list l)

-- | /O(l m)/, where /l/ is the cycle length and /m/ is the index of the start of the cycle. If the list contains no cycles, then the runtime is /O(n)/.
has_cycles :: (Eq a) => [a] -> Bool
has_cycles l = (decyclify_once l) /= l

-- ---------------------------------------------------------------------
-- Operations with two lists

-- | Given two infinite sorted lists, generates a list of elements in the first but not the second. Implementation from <http://en.literateprograms.org/Sieve_of_Eratosthenes_(Haskell)>.
diff_infinite :: (Ord a) => [a] -> [a] -> [a]
diff_infinite xs@(x:xt) ys@(y:yt) = 
    case compare x y of
        LT -> x : (diff_infinite xt ys)
        EQ -> diff_infinite xt yt
        GT -> diff_infinite xs yt

-- | /O(max(n, m))/ Merges the two given sorted lists of respective lengths /n/ and /m/. A special case of `merge_by` where the comparison function is `compare`.
merge :: (Ord a) => [a] -> [a] -> [a]
merge = merge_by compare

-- | /O(max(n, m))/ Merges the two given sorted lists of respective lengths /n/ and /m/, comparing elements in between the two lists with the given comparator function.
merge_by :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge_by cmp as bs
    | length as == 0 = bs
    | length bs == 0 = as
    | otherwise =
        let
            a = head as
            b = head bs
            as' = tail as
            bs' = tail bs
        in
            case cmp a b of
                    LT -> a : merge_by cmp as' bs
                    EQ -> a : merge_by cmp as' bs
                    GT -> b : merge_by cmp as  bs'


-- | /O(min(n, m))/ Zips the two given lists of respective lengths /n/ and /m/ as long as the pairs satisfy the given predicate function.
zip_while :: (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
zip_while f as bs = takeWhile (\(a, b) -> f a b) $ zip as bs

-- ---------------------------------------------------------------------
-- Sublists

-- | /O(n)/ Removes an element at the specified index in the given list.
remove_at_index :: Integer -> [a] -> [a]
remove_at_index i l =
    let a = fst $ splitAt (fromInteger i) l 
        b = snd $ splitAt (fromInteger i) l 
    in  a ++ tail b

-- | /O(n)/ Returns the subsequence of the given length at starting at index /i/ of length /m/. For example,
-- 
-- > subseq 4 5 [1..20] == [5,6,7,8,9]
subseq :: Integer -> Integer -> [a] -> [a]
subseq i len = take (fromInteger len) . drop (fromInteger i)

-- | /(O(n))/ Identical to `takeWhile`, but also contains the first element to satisfy the given predicate function. For example:
-- 
-- > take_while_keep_last (<3) [1..] == [1,2,3]
take_while_keep_last :: (a -> Bool) -> [a] -> [a]
take_while_keep_last f [] = []
take_while_keep_last f (x:xs) =
    if f x
        then [x]
        else x : take_while_keep_last f xs

-- | /(O(n))/ Returns a pair where the first element is identical to what `takeWhile` returns and the second element is the rest of the list
-- 
-- > take_while_and_rest (<3) [1..10] == ([1,2],[3,4,5,6,7,8,9,10])
take_while_and_rest :: (a -> Bool) -> [a] -> ([a], [a])
take_while_and_rest f [] = ([], [])
take_while_and_rest f l@(x:xs) = if not . f $ x
    then ([], l)
    else (x:(fst rec), snd rec)
    where
        rec = take_while_and_rest f xs

-- | /O(n)/ Like @Data.List.Find@, but returns a Maybe 2-tuple, instead, where the second element of the pair is the elements in the list after the first element of the pair.
--
-- > (find_and_rest ((==) 3) [1..10]) == Just (3, [4..10])
find_and_rest :: (a -> Bool) -> [a] -> Maybe (a, [a])
find_and_rest _ [] = Nothing
find_and_rest f (x:xs) = if f x
    then Just (x, xs)
    else find_and_rest f xs

-- | /(O(n^2))/ Returns all contiguous subsequences.
contiguous_subsequences :: [a] -> [[a]]
contiguous_subsequences = (:) [] . concatMap (tail . List.inits) . List.tails

-- | /(O(2^n))/ Returns all subsequences (contiguous and noncontiguous)
subsequences :: [a] -> [[a]]
subsequences = map reverse . powerset

-- | /O(n)/ Retuns all contiguous subsequences of the given length. E.g.:
--
--     > contiguous_subsequences_of_length 3 "1234567890"
--     ["123","234","345","456","567","678","789","890"]
contiguous_subsequences_of_length :: (Show a) => Integer -> [a] -> [[a]]
contiguous_subsequences_of_length len
    = contiguous_subsequences_of_length' len []
    where
        contiguous_subsequences_of_length' :: forall a. (Show a) => Integer -> [[a]] -> [a] -> [[a]]
        contiguous_subsequences_of_length' len so_far []
            = filter ((==) len . length') so_far
        contiguous_subsequences_of_length' len so_far (x:xs)
            = of_length ++ (contiguous_subsequences_of_length' len not_of_length xs)
            where
                of_length :: [[a]]
                of_length = filter ((==) len . length') so_far

                not_of_length :: [[a]]
                not_of_length
                    = map (flip (++) $ [x])
                    . (:) []
                    $ (filter ((/=) len . length') so_far)



-- ---------------------------------------------------------------------
-- Sorting

-- | /O(n)/ Returns whether the given list is sorted.
is_sorted :: (Ord a) => [a] -> Bool
is_sorted l = and $ zipWith (<=) l (tail l)

-- | /O(n log(n))/ Sorts the given list.
mergesort :: (Ord a) => [a] -> [a]
mergesort l =
    if length l <= 1
        then l
        else merge (mergesort a) (mergesort b)
        where
            (a, b) = splitAt (((fromIntegral $ length l) `div` 2)) l

-- ---------------------------------------------------------------------
-- Predicates

-- | /O(n)/ Returns whether the given list is a palindrome.
is_palindrome :: (Eq e) => [e] -> Bool
is_palindrome l = l == (reverse l)

-- TODO: do this more monadically?
-- | /O(n log(n))/ Returns whether the given list contains any element more than once.
contains_duplicates :: forall a . (Ord a) => [a] -> Bool
contains_duplicates l =
    if is_sorted l
        then or $ zipWith (==) l (tail l)
        else isNothing $ foldr insert (Just Set.empty) l
    where
        insert :: a -> Maybe (Set.Set a) -> Maybe (Set.Set a)
        insert a s = if isNothing s
            then Nothing
            else if Set.member a (fromJust s)
                then Nothing
                else Just (Set.insert a (fromJust s))

-- ---------------------------------------------------------------------
-- Assorted functions

-- | /O(nlog(n))/ Counts the number of time each element appears in the given list. For example:
--
--  > elem_counts [1,2,1,4] == [(1,2),(2,1),(4,1)]
elem_counts :: (Ord a) => [a] -> [(a, Integer)]
elem_counts
    = map (\l -> (head l, length' l))
    . List.group
    . List.sort

-- | /O(nlog(n))/ Counts the number of time each element appears in the given list. For example:
--
--  > elem_counts [1,2,1,4] == [(1,2),(2,1),(4,1)]
elem_counts_by :: (Ord b) => (a -> b) -> [a] -> [(a, Integer)]
elem_counts_by cmp
    = map (\l -> (head l, length' l))
    . List.groupBy (\a b -> cmp a == cmp b)
    . List.sortBy (Ord.comparing cmp)

-- | Shorthand for applying the same parameter twice.
--
--  > f $$ x = f x x
($$) :: (a -> a -> b) -> a -> b
f $$ a = f a a

-- | Shorthand for applicative functors:
--
--  > f <$*> l = f <$> l <*> l
(<$*>) :: Applicative f => (a -> a -> b) -> f a -> f b
f <$*> l = f <$> l <*> l

-- | /O(f log k)/, where k is the returnvalue, and f is the runtime of the input function on the lowest power of 2 above the returnvalue.
bsearch :: (Integer -> Ordering) -> Maybe Integer
bsearch f = bsearch' f lb ub
    where
        lb :: Integer
        lb
            = last
            . takeWhile (\n -> (f n) == LT)
            . map (\e -> 2^e)
            $ [0..]

        ub :: Integer
        ub = lb * 2

        bsearch' :: (Integer -> Ordering) -> Integer -> Integer -> Maybe Integer
        bsearch' f lb ub
            | (lb - ub <= 10)
                = List.find ((==) EQ . f)
                $ [lb..ub]
            | otherwise =
                case f curr of
                    LT -> bsearch' f curr ub
                    EQ -> Just curr
                    GT -> bsearch' f lb curr
                where
                    curr :: Integer
                    curr = (lb + ub) `div` 2

-- | /O(f log k)/, where k is the returnvalue, and f is the runtime of the input function on the lowest power of 2 above the returnvalue.
bsearch_1st_geq :: (Integer -> Ordering) -> Maybe Integer
bsearch_1st_geq f = bsearch_1st_geq' f lb ub
    where
        lb :: Integer
        lb
            = last
            . takeWhile (\n -> (f n) == LT)
            . map (\e -> 2^e)
            $ [0..]

        ub :: Integer
        ub = lb * 2

        bsearch_1st_geq' :: (Integer -> Ordering) -> Integer -> Integer -> Maybe Integer
        bsearch_1st_geq' f lb ub
            | (lb - ub <= 10)
                = List.find (\x -> ((==) EQ . f $ x) || ((==) GT . f $ x))
                $ [lb..ub]
            | otherwise =
                case f curr of
                    LT -> bsearch_1st_geq' f curr ub
                    EQ -> bsearch_1st_geq' f lb curr
                    GT -> bsearch_1st_geq' f lb curr
                where
                    curr :: Integer
                    curr = (lb + ub) `div` 2

-- | /O(n)/ Returns the noncontiguous sublist of elements greater than all previous elements. For example:
--
--   > running_bests [1,3,2,4,6,5] == [1,3,4,6]
running_bests :: forall a. (Ord a) => [a] -> [a]
running_bests = running_bests_by compare

-- | /O(n)/ Returns the noncontiguous sublist of elements greater than all previous elements, where "greater" is determined by the provided comparison function. For example:
--
--   > running_bests_by (Data.Ord.comparing length) [[1],[3,3,3],[2,2]] == [[1],[3,3,3]]
running_bests_by :: forall a. (Ord a) => (a -> a -> Ordering) -> [a] -> [a]
running_bests_by cmp [] = []
running_bests_by cmp (x:xs) = (:) x $ running_bests_by' xs x
    where
        running_bests_by' :: (Ord a) => [a] -> a -> [a]
        running_bests_by' [] _ = []
        running_bests_by' (x:xs) best_so_far = 
            case best_so_far `cmp` x of
                LT -> x : (running_bests_by' xs x)
                EQ -> x : (running_bests_by' xs x)
                GT ->      running_bests_by' xs best_so_far

-- | /O(min(n, m))/ Interleaves elements from the two given lists of respective lengths `n` and `m` in an alternating fashion. For example:
--
--  > interleave [1,3,5,7] [2,4,6,8] == [1,2,3,4,5,6,7,8]
--
--  > interleave [1,3,5,7] [2,4,6] == [1,2,3,4,5,6,7]
--
--  > interleave [1,3,5] [2,4,6,8] == [1,2,3,4,5,6,8]
interleave :: [a] -> [a] -> [a]
interleave [] bs = bs
interleave as [] = as
interleave (a:as) (b:bs) = a : b : (interleave as bs)

-- | /O(nf)/ Filters a list of length `n` leaving elemnts the indices of which satisfy the given predicate function, which has runtime `f`.
passing_index_elems :: (Int -> Bool) -> [a] -> [a]
passing_index_elems f
    = map snd
    . filter (f . fst)
    . zip [0..]

-- | /O(n)/ counts the number of elements in a list that satisfy a given predicate function.
count :: (a -> Bool) -> [a] -> Integer
count f = toInteger . length . filter f

-- | /O(n)/ Maps the given function over the list while keeping the original list. For example:
-- 
-- > map_keep chr [97..100] == [(97,'a'),(98,'b'),(99,'c'),(100,'d')]
map_keep :: (a -> b) -> [a] -> [(a, b)]
map_keep f l = zipWith (\a b -> (a, b)) l (map f l)

-- | Like `length`, but returns an integer.
length' :: [a] -> Integer
length' = toInteger . length

-- | Like `drop`, but takes an integer.
drop' :: Integer -> [a] -> [a]
drop' = drop . fromInteger

-- | Like `take`, but takes an integer.
take' :: Integer -> [a] -> [a]
take' = take . fromInteger

-- | List pre-pending.
cons :: a -> [a] -> [a]
cons = (:)

-- | List appending.
--
--   > snoc 4 [1,2,3] == [1,2,3,4]
snoc :: a -> [a] -> [a]
snoc e l = l ++ [e]

-- | /O(n)/ Finds the maximum element of the given list and returns a pair of it and the index at which it occurs (if the maximum element occurs multiple times, behavior is identical to that of `Data.List.maximumBy`). The list must be finite and non-empty.
maximum_with_index :: (Ord a) => [a] -> (a, Integer)
maximum_with_index xs =
    List.maximumBy (Ord.comparing fst) (zip xs [0..])

-- | /O(n)/ Finds the minimum element of the given list and returns a pair of it and the index at which it occurs (if the minimum element occurs multiple times, behavior is identical to that of `Data.List.minimumBy`). The list must be finite and non-empty.
minimum_with_index :: (Ord a) => [a] -> (a, Integer)
minimum_with_index xs =
    List.minimumBy (Ord.comparing fst) (zip xs [0..])

-- | /O(n)/ Finds all minima of the given list by the given comparator function. For example,
--   > minima_by (Data.Ord.comparing length) [[1,2], [1], [3,3,3], [2]]
--   [[1], [2]]
minima_by :: (a -> a -> Ordering) -> [a] -> [a]
minima_by cmp [] = []
minima_by cmp (x:xs) = minima_by' cmp xs [x]
    where
        minima_by' :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
        minima_by' _ [] so_far = so_far
        minima_by' cmp (x:xs) so_far =
            case x `cmp` (head so_far) of
                LT -> minima_by' cmp xs [x]
                EQ -> minima_by' cmp xs (x : so_far)
                GT -> minima_by' cmp xs so_far

-- | /O(n)/ Finds all maxima of the given list by the given comparator function. For example,
--   > maxima_by (Data.Ord.comparing length) [[1,2], [1], [3,3], [2]]
--   [[1,2], [3,3]]
maxima_by :: (a -> a -> Ordering) -> [a] -> [a]
maxima_by cmp = minima_by (flip cmp)

-- ---------------------------------------------------------------------
-- Tuples

-- | Applies the given function to the first element of the tuple.
map_fst :: (a -> c) -> (a, b) -> (c, b)
map_fst = flip map_pair $ id

-- | Applies the given function to the second element of the tuple.
map_snd :: (b -> c) -> (a, b) -> (a, c)
map_snd = map_pair id

-- | Applies the given two functions to the respective first and second elements of the tuple.
map_pair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
map_pair f g (a, b) = (f a, g b)

-- | Applies the given function to the first and second elements of the tuple.
map_pair_same :: (a -> b) -> (a, a) -> (b, b)
map_pair_same f (a, b) = (f a, f b)

-- | Applies the given function to respectively the first and second elements of the two tuple. For example,
-- 
--     > zip_with_pair (*) (^) (2,3) (5,4) == (10,27)
zip_with_pair :: (a -> c -> e) -> (b -> d -> f) -> (a, b) -> (c, d) -> (e, f)
zip_with_pair f g (a, b) (c, d) = (f a c, g b d)

-- | Like `zip_with_pair`, but re-using the same function.
zip_with_pair_same :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
zip_with_pair_same f (a, a') (b, b') = (f a b, f a' b')

-- | Applies the given three functions to the respective first, second, and third elements of the tuple.
map_triple :: (a -> d) -> (b -> e) -> (c -> f) -> (a, b, c) -> (d, e, f)
map_triple f g h (a, b, c) = (f a, g b, h c)

-- | Extracts the first element of a 3-tuple.
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- | Extracts the second element of a 3-tuple.
snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

-- | Extracts the third element of a 3-tuple.
trd3 :: (a, b, c) -> c
trd3 (_, _, c) = c

-- | Applies the given binary function to both elements of the given tuple.
pair_op :: (a -> b -> c) -> (a, b) -> c
pair_op op (a, b) = op a b

-- | Applies the given ternary function to all three elements of the given tuple.
triple_op :: (a -> b -> c -> d) -> (a, b, c) -> d
triple_op op (a, b, c) = op a b c
