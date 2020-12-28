-- |
-- Module      :  DobutokO.Poetry.Languages.General.Debug
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to order the 7 or less words (or their concatenations) 
-- to obtain (to some extent) suitable for poetry or music text. 
-- Functions in this module behaves just like their corresponding from the 
-- 'DobutokO.Poetry.Languages.General' module with that difference of being more 
-- informative and printing more output with the lists and 'V.Vector'.
-- Because of that, printing the output on the one line is not needed and 
-- therefore the corresponding one line functions are omitted.
-- Can be useful for debugging and exploration purposes. 
-- For all the used conversion functions of the type @g :: String -> Vector String@ 
-- it is important that they are stable for the repeated application (their result after 
-- the first application cannon be changed by the rules in the function into new variants). 
-- Otherwise, the recursive scheme of the functions in the module will lead to wrong results.
-- So the conversion function should work the following way (@xs@ denotes a word in the language) in GHCi: 
--
-- > let v = g xs
-- > let ys = concat . toList $ v 
-- > let v2 = g ys
-- > v == v2
-- > True
-- 
-- Or in the other words, for the single word, @g . concat . toList . g = g@;
-- 

module DobutokO.Poetry.Languages.General.Debug where

import Data.Maybe (fromJust)
import Data.Char (isPunctuation)
import qualified Data.Vector as V
import String.UniquenessPeriodsG (uniquenessPeriods)
import DobutokO.Poetry.Norms
import DobutokO.Poetry.Norms.Extended
import DobutokO.Poetry.Auxiliary
import DobutokO.Poetry.Languages.UniquenessPeriodsG
import DobutokO.Poetry.StrictV
import DobutokO.Poetry.Data

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqInMaxPoeticalN', but prints more information for debugging and tuning purposes. 
uniqInMaxPoeticalN :: Int -> V.Vector ([Int] -> Int) -> UniqG -> IO UniqG
uniqInMaxPoeticalN k vN x = do
  inner1 k vN x >>= \(fsT,x) -> 
    if isU x then return (U (V.filter (\(xs,_,_) -> xs /= fsT) . snd . get2 $ x))
    else return (UL ((\(v1,v2) -> ((V.toList . V.map (filter (not . isPunctuation) . lastFrom3) $ v1) ++ (fromJust . fst . get2 $ x),v2)) . 
      V.unstablePartition (\(xs,_,_) -> xs == fsT) . snd . get2 $ x))
{-# INLINE uniqInMaxPoeticalN #-}

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.inner1', but prints more information for debugging and tuning purposes. 
inner1 :: Int -> V.Vector ([Int] -> Int) -> UniqG -> IO ([Int],UniqG)
inner1 k vN x = do 
  let uniq = uniqMaxPoeticalGNV k vN x
  let fsT = (\(ys,_,_) -> ys) uniq
  putStrLn (filter (not . isPunctuation) . lastFrom3 $ uniq)
  putStrLn . show . firstFrom3 $ uniq
  putStrLn . show . secondFrom3 $ uniq
  return (fsT,x)
{-# INLINE inner1 #-}  

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqInMaxPoeticalNL', but prints more information for debugging and tuning purposes. 
uniqInMaxPoeticalNL :: V.Vector ([Int] -> Int) -> UniqG -> IO UniqG
uniqInMaxPoeticalNL vN x = uniqInMaxPoeticalN (V.length vN) vN x
{-# INLINE uniqInMaxPoeticalNL #-}

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqNPoeticalN', but prints more information for debugging and tuning purposes. 
uniqNPoeticalN :: Int -> Int -> V.Vector ([Int] -> Int) -> UniqG -> IO ()
uniqNPoeticalN n k vN y  
 | n <= 0 = return ()
 | compare (V.length . snd . get2 $ y) n == LT = V.mapM_ (\x -> do 
   { putStrLn (filter (not . isPunctuation) . lastFrom3 $ x) 
   ; putStrLn . show . firstFrom3 $ x
   ; putStrLn . show . secondFrom3 $ x}) . snd . get2 $ y
 | otherwise = (uniqInMaxPoeticalN k vN y >>= uniqNPoeticalN (n - 1) k vN)
{-# INLINE uniqNPoeticalN #-}

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqNPoeticalNL', but prints more information for debugging and tuning purposes. 
uniqNPoeticalNL :: Int -> V.Vector ([Int] -> Int) -> UniqG -> IO ()
uniqNPoeticalNL n vN = uniqNPoeticalN n (V.length vN) vN
{-# INLINE uniqNPoeticalNL #-}

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqNPoeticalVN', but prints more information for debugging and tuning purposes. 
uniqNPoeticalVN :: Int -> Int -> V.Vector ([Int] -> Int) -> UniqG -> IO UniqG
uniqNPoeticalVN n k vN y
 | n <= 0 || compare (V.length . snd . get2 $ y) n == LT = return y
 | otherwise = (uniqInMaxPoeticalN k vN y >>= uniqNPoeticalVN (n - 1) k vN)
{-# INLINE uniqNPoeticalVN #-}

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqNPoeticalVNL', but prints more information for debugging and tuning purposes. 
uniqNPoeticalVNL :: Int -> V.Vector ([Int] -> Int) -> UniqG -> IO UniqG
uniqNPoeticalVNL n vN = uniqNPoeticalVN n (V.length vN) vN
{-# INLINE uniqNPoeticalVNL #-}

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqMaxPoeticalGNV', but prints more information for debugging and tuning purposes. 
uniqMaxPoeticalGNV :: Int -> V.Vector ([Int] -> Int) ->  UniqG -> Uniqueness
uniqMaxPoeticalGNV k vN y
 | compare k (V.length vN) == GT = error "DobutokO.Poetry.Languages.General.Debug.uniqMaxPoeticalGNV: undefined for that amount of norms. "
 | compare k 0 == GT =
   let maxK = V.maximumBy (\(_,vN0,_) (_,vN1,_) -> compare (V.unsafeIndex vN0 (k - 1)) (V.unsafeIndex vN1 (k - 1))) . snd . get2 $ y
       vK = V.filter (\(_,vN2,_) -> V.unsafeIndex vN2 (k - 1) == ((\(_,vNk,_) -> V.unsafeIndex vNk (k - 1)) maxK)) . snd . get2 $ y in
         if isU y then uniqMaxPoeticalGNV (k - 1) (V.unsafeSlice 0 (V.length vN - 1) vN) (U vK)
         else uniqMaxPoeticalGNV (k - 1) (V.unsafeSlice 0 (V.length vN - 1) vN) (UL (fromJust . fst . get2 $ y,vK))
 | otherwise = V.maximumBy (\(_,vN0,_) (_,vN1,_) -> compare (V.unsafeIndex vN0 0) (V.unsafeIndex vN1 0)) . snd . get2 $ y
{-# INLINE uniqMaxPoeticalGNV #-}

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqMaxPoeticalGNVL', but prints more information for debugging and tuning purposes. 
uniqMaxPoeticalGNVL :: V.Vector ([Int] -> Int) ->  UniqG -> Uniqueness
uniqMaxPoeticalGNVL vN = uniqMaxPoeticalGNV (V.length vN) vN
{-# INLINE uniqMaxPoeticalGNVL #-}

---------------------------------------------------------------------------------

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniquenessVariantsGN', but prints more information for debugging and tuning purposes. 
uniquenessVariantsGN :: [String] -> (String -> V.Vector String) -> Preapp -> V.Vector ([Int] -> Int) -> String -> V.Vector Uniqueness
uniquenessVariantsGN whspss g (PA ts us) vN = uniquenessVariants2GNP ts us vN (uniquenessPeriods whspss g)
uniquenessVariantsGN whspss g K vN = uniquenessVariants2GN vN (uniquenessPeriods whspss g)
{-# INLINE uniquenessVariantsGN #-}

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqMaxPoeticalGN', but prints more information for debugging and tuning purposes. 
uniqMaxPoeticalGN :: [String] -> (String -> V.Vector String) -> Preapp -> Int -> V.Vector ([Int] -> Int) ->  String -> Uniqueness
uniqMaxPoeticalGN whspss g x k vN = uniqMaxPoetical2GN whspss g x k vN (uniquenessPeriods whspss g)
{-# INLINE uniqMaxPoeticalGN #-}

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqMaxPoeticalGNL', but prints more information for debugging and tuning purposes. 
uniqMaxPoeticalGNL :: [String] -> (String -> V.Vector String) -> Preapp -> V.Vector ([Int] -> Int) ->  String -> Uniqueness
uniqMaxPoeticalGNL whspss g x vN = uniqMaxPoeticalGN whspss g x (V.length vN) vN
{-# INLINE uniqMaxPoeticalGNL #-}

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqNPoeticalG', but prints more information for debugging and tuning purposes. 
uniqNPoeticalG :: [String] -> (String -> V.Vector String) -> Preapp -> Int -> ([Int] -> Int) -> String -> IO ()
uniqNPoeticalG whspss g x n h = uniqNPoeticalGN whspss g x n 1 (V.singleton h)
{-# INLINE uniqNPoeticalG #-}

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniq10PoeticalG', but prints more information for debugging and tuning purposes. 
uniq10PoeticalG :: [String] -> (String -> V.Vector String) -> Preapp -> ([Int] -> Int) -> String -> IO ()
uniq10PoeticalG whspss g x = uniqNPoeticalG whspss g x 10
{-# INLINE uniq10PoeticalG #-}

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniq10Poetical4', but prints more information for debugging and tuning purposes. 
uniq10Poetical4 :: [String] -> (String -> V.Vector String) -> Preapp -> String -> IO ()
uniq10Poetical4 whspss g x = uniq10PoeticalG whspss g x norm4
{-# INLINE uniq10Poetical4 #-}

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniq10Poetical5', but prints more information for debugging and tuning purposes. 
uniq10Poetical5 :: [String] -> (String -> V.Vector String) -> Preapp -> String -> IO ()
uniq10Poetical5 whspss g x = uniq10PoeticalG whspss g x norm5
{-# INLINE uniq10Poetical5 #-}

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqNPoeticalGN', but prints more information for debugging and tuning purposes. 
uniqNPoeticalGN :: [String] -> (String -> V.Vector String) -> Preapp -> Int -> Int -> V.Vector ([Int] -> Int) -> String -> IO ()
uniqNPoeticalGN whspss g x n k vN = uniqNPoetical2GN x n k vN (uniquenessPeriods whspss g)
{-# INLINE uniqNPoeticalGN #-}

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqNPoeticalGNL', but prints more information for debugging and tuning purposes. 
uniqNPoeticalGNL :: [String] -> (String -> V.Vector String) -> Preapp -> Int -> V.Vector ([Int] -> Int) -> String -> IO ()
uniqNPoeticalGNL whspss g x n vN = uniqNPoetical2GNL x n vN (uniquenessPeriods whspss g)
{-# INLINE uniqNPoeticalGNL #-}

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqNPoeticalVGN', but prints more information for debugging and tuning purposes. 
uniqNPoeticalVGN :: [String] -> (String -> V.Vector String) -> Preapp -> Int -> Int -> V.Vector ([Int] -> Int) -> UniqG -> String -> IO UniqG
uniqNPoeticalVGN whspss g x n k vN = uniqNPoetical2VGN x n k vN (uniquenessPeriods whspss g)
{-# INLINE uniqNPoeticalVGN #-}

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqNPoeticalVGNL', but prints more information for debugging and tuning purposes. 
uniqNPoeticalVGNL :: [String] -> (String -> V.Vector String) -> Preapp -> Int -> V.Vector ([Int] -> Int) -> UniqG -> String -> IO UniqG
uniqNPoeticalVGNL whspss g x n vN = uniqNPoetical2VGN x n (V.length vN) vN (uniquenessPeriods whspss g)
{-# INLINE uniqNPoeticalVGNL #-}

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqMaxPoetical2GN', but prints more information for debugging and tuning purposes. 
uniqMaxPoetical2GN :: [String] -> (String -> V.Vector String) -> Preapp -> Int -> V.Vector ([Int] -> Int) ->  (String -> [Int]) -> String -> Uniqueness
uniqMaxPoetical2GN whspss h rr k vN g xs
 | compare k (V.length vN) == GT = error "DobutokO.Poetry.Languages.General.Debug.uniqMaxPoetical2GN: undefined for that amount of norms. "
 | compare k 0 == GT =
   let vM = uniquenessVariants2GNP (get1m rr) (get2m rr) vN g xs
       maxK = V.maximumBy (\(_,vN0,_) (_,vN1,_) -> compare (V.unsafeIndex vN0 (k - 1)) (V.unsafeIndex vN1 (k - 1))) vM
       vK = V.filter (\(_,vN2,_) -> V.unsafeIndex vN2 (k - 1) == ((\(_,vNk,_) -> V.unsafeIndex vNk (k - 1)) maxK)) vM in
         uniqMaxPoeticalGNV (k - 1) (V.unsafeSlice 0 (V.length vN - 1) vN) (U vK)
 | otherwise = V.maximumBy (\(_,vN0,_) (_,vN1,_) -> compare (V.unsafeIndex vN0 0) (V.unsafeIndex vN1 0)) . uniquenessVariantsGN whspss h rr vN $ xs

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqMaxPoetical2GNL', but prints more information for debugging and tuning purposes. 
uniqMaxPoetical2GNL :: [String] -> (String -> V.Vector String) -> Preapp -> V.Vector ([Int] -> Int) ->  (String -> [Int]) -> String -> Uniqueness
uniqMaxPoetical2GNL whspss g rr vN = uniqMaxPoetical2GN whspss g rr (V.length vN) vN
{-# INLINE uniqMaxPoetical2GNL #-}
  
-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqNPoetical2GN', but prints more information for debugging and tuning purposes. 
uniqNPoetical2GN :: Preapp -> Int -> Int -> V.Vector ([Int] -> Int) -> (String -> [Int]) -> String -> IO ()
uniqNPoetical2GN rr n k vN g xs
 | n <= 0 = return ()
 | otherwise = do
   let v = uniquenessVariants2GNP (get1m rr) (get2m rr) vN g xs
   if compare (V.length v) n == LT
     then V.mapM_ (\x -> do 
       { putStrLn ((filter (not . isPunctuation) . lastFrom3 $ x)) 
       ; putStrLn . show . firstFrom3 $ x
       ; putStrLn . show . secondFrom3 $ x}) v
     else (uniqInMaxPoeticalN k vN (U v) >>= uniqNPoeticalN (n - 1) k vN)

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqNPoetical2GNL', but prints more information for debugging and tuning purposes. 
uniqNPoetical2GNL :: Preapp -> Int -> V.Vector ([Int] -> Int) -> (String -> [Int]) -> String -> IO ()
uniqNPoetical2GNL rr n vN = uniqNPoetical2GN rr n (V.length vN) vN
{-# INLINE uniqNPoetical2GNL #-}
 
-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqNPoetical2VGN', but prints more information for debugging and tuning purposes. 
uniqNPoetical2VGN :: Preapp -> Int -> Int -> V.Vector ([Int] -> Int) -> (String -> [Int]) -> UniqG -> String -> IO UniqG
uniqNPoetical2VGN rr n k vN g y xs
 | n <= 0 = if isU y then return (U V.empty) else return (UL ([],V.empty))
 | otherwise = do
   let v = uniquenessVariants2GNP (get1m rr) (get2m rr) vN g xs
   if compare (V.length v) n == LT 
     then if isU y then return (U v) else return (UL ([],v)) 
     else if isU y then uniqNPoeticalVN n k vN (U v) else uniqNPoeticalVN n k vN (UL ([],v))

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqNPoetical2VGNL', but prints more information for debugging and tuning purposes. 
uniqNPoetical2VGNL :: Preapp -> Int -> V.Vector ([Int] -> Int) -> (String -> [Int]) -> UniqG -> String -> IO UniqG
uniqNPoetical2VGNL rr n vN = uniqNPoetical2VGN rr n (V.length vN) vN
{-# INLINE uniqNPoetical2VGNL #-}
 
-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqNPoeticalUGN_', but prints more information for debugging and tuning purposes. 
uniqNPoeticalUGN_ :: [String] -> (String -> V.Vector String) -> Preapp -> Int -> Int -> Int -> V.Vector ([Int] -> Int) -> String -> IO ()
uniqNPoeticalUGN_ whspss g rr x n k vN = uniqNPoetical2GN rr n k vN (uniquenessPeriods2 whspss g x)
{-# INLINE uniqNPoeticalUGN_ #-}

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqNPoeticalUGNL_', but prints more information for debugging and tuning purposes. 
uniqNPoeticalUGNL_ :: [String] -> (String -> V.Vector String) -> Preapp -> Int -> Int -> V.Vector ([Int] -> Int) -> String -> IO ()
uniqNPoeticalUGNL_ whspss g rr x n vN = uniqNPoetical2GNL rr n vN (uniquenessPeriods2 whspss g x)
{-# INLINE uniqNPoeticalUGNL_ #-}

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqNPoeticalUGN', but prints more information for debugging and tuning purposes. 
uniqNPoeticalUGN :: [String] -> (String -> V.Vector String) -> Preapp -> Int -> Int -> Int -> V.Vector ([Int] -> Int) -> UniqG -> String -> IO UniqG
uniqNPoeticalUGN whspss g rr x n k vN = uniqNPoetical2VGN rr n k vN (uniquenessPeriods2 whspss g x)
{-# INLINE uniqNPoeticalUGN #-}

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqNPoeticalUGNL', but prints more information for debugging and tuning purposes. 
uniqNPoeticalUGNL :: [String] -> (String -> V.Vector String) -> Preapp -> Int -> Int -> V.Vector ([Int] -> Int) -> UniqG -> String -> IO UniqG
uniqNPoeticalUGNL whspss g rr x n vN = uniqNPoetical2VGN rr n (V.length vN) vN (uniquenessPeriods2 whspss g x)
{-# INLINE uniqNPoeticalUGNL #-}

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqNPoeticalUGN51_', but prints more information for debugging and tuning purposes. 
uniqNPoeticalUGN51_ :: [String] -> (String -> V.Vector String) -> Preapp -> Int -> Int -> String -> IO ()
uniqNPoeticalUGN51_ whspss g rr x n = uniqNPoeticalUGN_ whspss g rr x n 1 (V.singleton norm51)
{-# INLINE uniqNPoeticalUGN51_ #-}

-- | Behaves like the function 'DobutokO.Poetry.Languages.General.uniqNPoeticalUGN51', but prints more information for debugging and tuning purposes. 
uniqNPoeticalUGN51 :: [String] -> (String -> V.Vector String) -> Preapp -> Int -> Int -> UniqG -> String -> IO UniqG
uniqNPoeticalUGN51 whspss g rr x n = uniqNPoeticalUGN whspss g rr x n 1 (V.singleton norm51)
{-# INLINE uniqNPoeticalUGN51 #-}
