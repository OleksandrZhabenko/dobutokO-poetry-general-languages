-- |
-- Module      :  DobutokO.Poetry.Languages.General
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to order the 7 or less words (or their concatenations) 
-- to obtain (to some extent) suitable for poetry or music text. 
-- The functions can only print the needed 
-- strings or also return tha data needed to interconnect and link it with 
-- other functions. There is also a possibility to use prepending and 
-- postpending 'String' in the 'PreApp' data type so that they are added 
-- respectively to the beginning or to the end of the strings.
-- For all the used conversion functions of the type @h :: String -> Vector String@ 
-- it is important that they are stable for the repeated application (their result after 
-- the first application cannon be changed by the rules in the function into new variants). 
-- Otherwise, the recursive scheme of the functions in the module will lead to wrong results.
-- So the conversion function should work the following way (@xs@ denotes a word in the language) in GHCi: 
--
-- > let v = h xs
-- > let ys = concat . toList $ v 
-- > let v2 = h ys
-- > v == v2
-- > True
-- 
-- Or in the other words, for the single word, @h . concat . toList . h = h@;
-- 

module DobutokO.Poetry.Languages.General where

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

-- | Prints the maximum element with respect of the @k@ norms (the most significant of which is the rightest one, then to the left less significant etc.), 
-- which is given as the first argument. The last norm is the first element in the 'V.Vector' of norms (@[Int] -> Int@). 
uniqInMaxPoeticalN :: Int -> V.Vector ([Int] -> Int) -> UniqG -> IO UniqG
uniqInMaxPoeticalN k vN x = do
  inner1 k vN x >>= \(fsT,x) -> 
    if isU x then return (U (V.filter (\(xs,_,_) -> xs /= fsT) . snd . get2 $ x))
    else return (UL ((\(v1,v2) -> ((V.toList . V.map (filter (not . isPunctuation) . lastFrom3) $ v1) ++ (fromJust . fst . get2 $ x),v2)) . 
      V.unstablePartition (\(xs,_,_) -> xs == fsT) . snd . get2 $ x))
{-# INLINE uniqInMaxPoeticalN #-}

-- | Is used internally in the 'uniqInMaxPoeticalN' to reduce duplication.
inner1 :: Int -> V.Vector ([Int] -> Int) -> UniqG -> IO ([Int],UniqG)
inner1 k vN x = do 
  let uniq = uniqMaxPoeticalGNV k vN x
  let fsT = (\(ys,_,_) -> ys) uniq
  putStr (filter (not . isPunctuation) . lastFrom3 $ uniq) >> putStrLn ""
  return (fsT,x)
{-# INLINE inner1 #-}  

-- | Variant of 'uniqInMaxPoticalN' where all the elements in the norms 'V.Vector' are used as norms from right to left.
uniqInMaxPoeticalNL :: V.Vector ([Int] -> Int) -> UniqG -> IO UniqG
uniqInMaxPoeticalNL vN x = uniqInMaxPoeticalN (V.length vN) vN x
{-# INLINE uniqInMaxPoeticalNL #-}

-- | Generalized variant of the 'uniqInMaxPoeticalN' with usage of the several norms and all the information is printed on the same line. 
uniqInMaxPoeticalNLine :: Int -> V.Vector ([Int] -> Int) -> UniqG -> IO UniqG
uniqInMaxPoeticalNLine k vN x = do
  inner2 k vN x >>= \(fsT,x) -> 
    if isU x then return (U (V.filter (\(xs,_,_) -> xs /= fsT) . snd . get2 $ x))
    else return (UL ((\(v1,v2) -> ((V.toList . V.map (filter (not . isPunctuation) . lastFrom3) $ v1) ++ (fromJust . fst . get2 $ x),v2)) . 
      V.unstablePartition (\(xs,_,_) -> xs == fsT) . snd . get2 $ x))
{-# INLINE uniqInMaxPoeticalNLine #-}

-- | Is used internally in the 'uniqInMaxPoeticalNLine' to reduce duplication.
inner2 :: Int -> V.Vector ([Int] -> Int) -> UniqG -> IO ([Int],UniqG)
inner2 k vN x = do 
  let uniq = uniqMaxPoeticalGNV k vN x
  let fsT = (\(ys,_,_) -> ys) uniq
  putStr (filter (not . isPunctuation) . lastFrom3 $ uniq) >> putStr " "
  return (fsT,x)
{-# INLINE inner2 #-}

-- | Variant of 'uniqInMaxPoticalNLine' where all the elements in the norms 'V.Vector' are used as norms from right to left.
uniqInMaxPoeticalNLineL :: V.Vector ([Int] -> Int) -> UniqG -> IO UniqG
uniqInMaxPoeticalNLineL vN = uniqInMaxPoeticalNLine (V.length vN) vN
{-# INLINE uniqInMaxPoeticalNLineL #-}

-- | Prints @n@ (given as the first argument) maximum elements with respect to the several norms (their quantity is the second argument) starting 
-- from the right to the left. The last norm is the first element in the 'V.Vector' of norms (@[Int] -> Int@). 
uniqNPoeticalN :: Int -> Int -> V.Vector ([Int] -> Int) -> UniqG -> IO ()
uniqNPoeticalN n k vN y  
 | n <= 0 = return ()
 | compare (V.length . snd . get2 $ y) n == LT = V.mapM_ (\x -> putStr (filter (not . isPunctuation) . lastFrom3 $ x) >> putStrLn "" ) . snd . get2 $ y
 | otherwise = (uniqInMaxPoeticalN k vN y >>= uniqNPoeticalN (n - 1) k vN)
{-# INLINE uniqNPoeticalN #-}

-- | Variant of 'uniqNPoeticalN' where all the elements in the norms 'V.Vector' are used as norms from right to left.
uniqNPoeticalNL :: Int -> V.Vector ([Int] -> Int) -> UniqG -> IO ()
uniqNPoeticalNL n vN = uniqNPoeticalN n (V.length vN) vN
{-# INLINE uniqNPoeticalNL #-}

-- | Variant of the 'uniqNPoeticalN' with its output being printed on the same line.
uniqNPoeticalNLine :: Int -> Int -> V.Vector ([Int] -> Int) -> UniqG -> IO ()
uniqNPoeticalNLine n k vN y
 | n <= 0 = putStrLn ""
 | compare (V.length . snd . get2 $ y) n == LT = 
    (V.mapM_ (\x -> putStr (filter (not . isPunctuation) . lastFrom3 $ x) >> putStr " " ) . snd . get2 $ y)  >> putStrLn ""
 | otherwise = (uniqInMaxPoeticalNLine k vN y >>= uniqNPoeticalNLine (n - 1) k vN)
{-# INLINE uniqNPoeticalNLine #-}
 
-- | Variant of 'uniqNPoeticalNLine' where all the elements in the norms 'V.Vector' are used as norms from right to left.
uniqNPoeticalNLineL :: Int -> V.Vector ([Int] -> Int) -> UniqG -> IO ()
uniqNPoeticalNLineL n vN = uniqNPoeticalNLine n (V.length vN) vN
{-# INLINE uniqNPoeticalNLineL #-}

-- | Prints @n@ (given as the first argument) maximum elements with respect to the several norms (their quantity is the second argument) starting 
-- from the right to the left. The last norm is the first element in the 'V.Vector' of norms (@[Int] -> Int@). Contrary to its pair function 
-- 'uniqNPoeticalN' returns then the rest of the given 'V.Vector' 'Uniqueness' after filtering the printed elements 'String'.
uniqNPoeticalVN :: Int -> Int -> V.Vector ([Int] -> Int) -> UniqG -> IO UniqG
uniqNPoeticalVN n k vN y
 | n <= 0 || compare (V.length . snd . get2 $ y) n == LT = return y
 | otherwise = (uniqInMaxPoeticalN k vN y >>= uniqNPoeticalVN (n - 1) k vN)
{-# INLINE uniqNPoeticalVN #-}

-- | Variant of 'uniqNPoeticalVN' where all the elements in the norms 'V.Vector' are used as norms from right to left.
uniqNPoeticalVNL :: Int -> V.Vector ([Int] -> Int) -> UniqG -> IO UniqG
uniqNPoeticalVNL n vN = uniqNPoeticalVN n (V.length vN) vN
{-# INLINE uniqNPoeticalVNL #-}

-- | The function evaluates the 'V.Vector' of 'Uniqueness' elements to retrieve the possibly maximum element in it with respect to the order 
-- and significance (principality)  of the norms being evaluated. The most significant and principal is the norm, which index in the 'V.Vector' of them is 
-- the 'Int' argument of the function minus 1, then less significant is the next to the left norm and so on. Is similar to 'DobutokO.Poetry.uniqMaxPoeticalGN' 
-- function.
uniqMaxPoeticalGNV :: Int -> V.Vector ([Int] -> Int) ->  UniqG -> Uniqueness
uniqMaxPoeticalGNV k vN y
 | compare k (V.length vN) == GT = error "DobutokO.Poetry.Languages.General.uniqMaxPoeticalGNV: undefined for that amount of norms. "
 | compare k 0 == GT =
   let maxK = V.maximumBy (\(_,vN0,_) (_,vN1,_) -> compare (V.unsafeIndex vN0 (k - 1)) (V.unsafeIndex vN1 (k - 1))) . snd . get2 $ y
       vK = V.filter (\(_,vN2,_) -> V.unsafeIndex vN2 (k - 1) == ((\(_,vNk,_) -> V.unsafeIndex vNk (k - 1)) maxK)) . snd . get2 $ y in
         if isU y then uniqMaxPoeticalGNV (k - 1) (V.unsafeSlice 0 (V.length vN - 1) vN) (U vK)
         else uniqMaxPoeticalGNV (k - 1) (V.unsafeSlice 0 (V.length vN - 1) vN) (UL (fromJust . fst . get2 $ y,vK))
 | otherwise = V.maximumBy (\(_,vN0,_) (_,vN1,_) -> compare (V.unsafeIndex vN0 0) (V.unsafeIndex vN1 0)) . snd . get2 $ y
{-# INLINE uniqMaxPoeticalGNV #-}

-- | Variant of 'uniqMaxPoeticalGNV' where all the elements in the norms 'V.Vector' are used as norms from right to left.
uniqMaxPoeticalGNVL :: V.Vector ([Int] -> Int) ->  UniqG -> Uniqueness
uniqMaxPoeticalGNVL vN = uniqMaxPoeticalGNV (V.length vN) vN
{-# INLINE uniqMaxPoeticalGNVL #-}

---------------------------------------------------------------------------------

-- | Returns the 'V.Vector' of all possible permutations of the 'String' that represent the text and the linked information with them for 
-- analysis  with usage of several norms (instead of one). They constitute a 'V.Vector' of functions 
-- @norm :: [Int] -> Int@. So the inner vector in the each resulting 'Uniqueness' has the same length as the vector of norms. 
-- Uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation). The second argument is a function that 
-- converts a 'String' of the text into the 'V.Vector' of sound representations for that language. 
uniquenessVariantsGN :: [String] -> (String -> V.Vector String) -> Preapp -> V.Vector ([Int] -> Int) -> String -> V.Vector Uniqueness
uniquenessVariantsGN whspss h (PA ts us) vN = uniquenessVariants2GNP ts us vN (uniquenessPeriods whspss h)
uniquenessVariantsGN whspss h K vN = uniquenessVariants2GN vN (uniquenessPeriods whspss h)
{-# INLINE uniquenessVariantsGN #-}

-- | A variant of the 'uniqMaxPoetical2GN' with the several norms given as a 'V.Vector' of functions and an 'Int' parameter. The function evaluates 
-- the generated 'V.Vector' of 'Uniqueness' elements to retrieve the possibly maximum element in it with respect to the order and significance (principality) 
-- of the norms being evaluated. The most significant and principal is the norm, which index in the 'V.Vector' of them is the 'Int' argument of the function 
-- minus 1, then less significant is the next to the left norm and so on.
-- Uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation). The second argument is a function that 
-- converts a 'String' of the text into the 'V.Vector' of sound representations for that language. 
uniqMaxPoeticalGN :: [String] -> (String -> V.Vector String) -> Preapp -> Int -> V.Vector ([Int] -> Int) ->  String -> Uniqueness
uniqMaxPoeticalGN whspss h x k vN = uniqMaxPoetical2GN whspss h x k vN (uniquenessPeriods whspss h)
{-# INLINE uniqMaxPoeticalGN #-}

-- | Variant of 'uniqMaxPoeticalGN' where all the elements in the norms 'V.Vector' are used as norms from right to left.
-- Uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation). The second argument is a function that 
-- converts a 'String' of the text into the 'V.Vector' of sound representations for that language. 
uniqMaxPoeticalGNL :: [String] -> (String -> V.Vector String) -> Preapp -> V.Vector ([Int] -> Int) ->  String -> Uniqueness
uniqMaxPoeticalGNL whspss h x vN = uniqMaxPoeticalGN whspss h x (V.length vN) vN
{-# INLINE uniqMaxPoeticalGNL #-}

-- | A variant of the 'uniqNPoeticalGN' with only one norm.
-- Uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation). The second argument is a function that 
-- converts a 'String' of the text into the 'V.Vector' of sound representations for that language. 
uniqNPoeticalG :: [String] -> (String -> V.Vector String) -> Preapp -> Int -> ([Int] -> Int) -> String -> IO ()
uniqNPoeticalG whspss h x n g = uniqNPoeticalGN whspss h x n 1 (V.singleton g)
{-# INLINE uniqNPoeticalG #-}

-- | A variant of the 'uniqNPoeticalG' function with the @n@ equal to 10.
-- Uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation). The second argument is a function that 
-- converts a 'String' of the text into the 'V.Vector' of sound representations for that language. 
uniq10PoeticalG :: [String] -> (String -> V.Vector String) -> Preapp -> ([Int] -> Int) -> String -> IO ()
uniq10PoeticalG whspss h x = uniqNPoeticalG whspss h x 10
{-# INLINE uniq10PoeticalG #-}

-- | A variant of 'uniq10PoeticalG' with the 'norm4' applied. The list is (according to some model, not universal, but a reasonable one in the most cases) the 
-- most suitable for intonation changing and, therefore, for the accompaniment of the highly changable or variative melody. 
-- Uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation). The second argument is a function that 
-- converts a 'String' of the text into the 'V.Vector' of sound representations for that language. 
uniq10Poetical4 :: [String] -> (String -> V.Vector String) -> Preapp -> String -> IO ()
uniq10Poetical4 whspss h x = uniq10PoeticalG whspss h x norm4
{-# INLINE uniq10Poetical4 #-}

-- | A variant of 'uniq10PoeticalG' with the 'norm5' applied. The list is (according to some model, not universal, but a reasonable one in the most cases) the 
-- most suitable for rhythmic speech and two-syllabilistic-based poetry. Therefore, it can be used to create a poetic composition or to emphasize some 
-- thoughts. 
-- Uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation). The second argument is a function that 
-- converts a 'String' of the text into the 'V.Vector' of sound representations for that language. 
uniq10Poetical5 :: [String] -> (String -> V.Vector String) -> Preapp -> String -> IO ()
uniq10Poetical5 whspss h x = uniq10PoeticalG whspss h x norm5
{-# INLINE uniq10Poetical5 #-}

-- | A variant of the 'uniqNPoetical2GN' with the conversion (\"uniquenessPeriods\" function) function 'uniquenessPeriods'.
-- Uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation). The second argument is a function that 
-- converts a 'String' of the text into the 'V.Vector' of sound representations for that language. 
uniqNPoeticalGN :: [String] -> (String -> V.Vector String) -> Preapp -> Int -> Int -> V.Vector ([Int] -> Int) -> String -> IO ()
uniqNPoeticalGN whspss h x n k vN = uniqNPoetical2GN x n k vN (uniquenessPeriods whspss h)
{-# INLINE uniqNPoeticalGN #-}

-- | Variant of 'uniqNPoeticalGN' where all the elements in the norms 'V.Vector' are used as norms from right to left. 
-- Uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation). The second argument is a function that 
-- converts a 'String' of the text into the 'V.Vector' of sound representations for that language. 
uniqNPoeticalGNL :: [String] -> (String -> V.Vector String) -> Preapp -> Int -> V.Vector ([Int] -> Int) -> String -> IO ()
uniqNPoeticalGNL whspss h x n vN = uniqNPoetical2GNL x n vN (uniquenessPeriods whspss h)
{-# INLINE uniqNPoeticalGNL #-}

-- | Generalized variant of the 'uniqNPoeticalVG' with usage of several norms. 
-- Uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- To specify whether the function returns also data suitable for usage with other functions or just usable mostly for printing it uses also a before 'String' 
-- argument also 'UniqG' one with the 'U' data constructor corresponding to the printing (mostly) and 'UL' to possible reusage of data.
-- The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation). The second argument is a function that 
-- converts a 'String' of the text into the 'V.Vector' of sound representations for that language. 
uniqNPoeticalVGN :: [String] -> (String -> V.Vector String) -> Preapp -> Int -> Int -> V.Vector ([Int] -> Int) -> UniqG -> String -> IO UniqG
uniqNPoeticalVGN whspss h x n k vN = uniqNPoetical2VGN x n k vN (uniquenessPeriods whspss h)
{-# INLINE uniqNPoeticalVGN #-}

-- | Variant of 'uniqNPoeticalVGN' where all the elements in the norms 'V.Vector' are used as norms from right to left.
-- Uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- To specify whether the function returns also data suitable for usage with other functions or just usable mostly for printing it uses also a before 'String' 
-- argument also 'UniqG' one with the 'U' data constructor corresponding to the printing (mostly) and 'UL' to possible reusage of data.
-- The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation). The second argument is a function that 
-- converts a 'String' of the text into the 'V.Vector' of sound representations for that language. 
uniqNPoeticalVGNL :: [String] -> (String -> V.Vector String) -> Preapp -> Int -> V.Vector ([Int] -> Int) -> UniqG -> String -> IO UniqG
uniqNPoeticalVGNL whspss h x n vN = uniqNPoetical2VGN x n (V.length vN) vN (uniquenessPeriods whspss h)
{-# INLINE uniqNPoeticalVGNL #-}

-- | The function evaluates 
-- the generated 'V.Vector' of 'Uniqueness' elements to retrieve the possibly maximum element in it with respect to the order and significance (principality) 
-- of the norms being evaluated. The most significant and principal is the norm, which index in the 'V.Vector' of them is the 'Int' argument of the function 
-- minus 1, then less significant is the next to the left norm and so on.
-- Uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation). The second argument is a function that 
-- converts a 'String' of the text into the 'V.Vector' of sound representations for that language. 
uniqMaxPoetical2GN :: [String] -> (String -> V.Vector String) -> Preapp -> Int -> V.Vector ([Int] -> Int) ->  (String -> [Int]) -> String -> Uniqueness
uniqMaxPoetical2GN whspss h rr k vN g xs
 | compare k (V.length vN) == GT = error "DobutokO.Poetry.Languages.General.uniqMaxPoetical2GN: undefined for that amount of norms. "
 | compare k 0 == GT =
   let vM = uniquenessVariants2GNP (get1m rr) (get2m rr) vN g xs
       maxK = V.maximumBy (\(_,vN0,_) (_,vN1,_) -> compare (V.unsafeIndex vN0 (k - 1)) (V.unsafeIndex vN1 (k - 1))) vM
       vK = V.filter (\(_,vN2,_) -> V.unsafeIndex vN2 (k - 1) == ((\(_,vNk,_) -> V.unsafeIndex vNk (k - 1)) maxK)) vM in
         uniqMaxPoeticalGNV (k - 1) (V.unsafeSlice 0 (V.length vN - 1) vN) (U vK)
 | otherwise = V.maximumBy (\(_,vN0,_) (_,vN1,_) -> compare (V.unsafeIndex vN0 0) (V.unsafeIndex vN1 0)) . uniquenessVariantsGN whspss h rr vN $ xs

-- | Variant of 'uniqMaxPoetical2GN' where all the elements in the norms 'V.Vector' are used as norms from right to left.
-- Uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation). The second argument is a function that 
-- converts a 'String' of the text into the 'V.Vector' of sound representations for that language. 
uniqMaxPoetical2GNL :: [String] -> (String -> V.Vector String) -> Preapp -> V.Vector ([Int] -> Int) ->  (String -> [Int]) -> String -> Uniqueness
uniqMaxPoetical2GNL whspss h rr vN = uniqMaxPoetical2GN whspss h rr (V.length vN) vN
{-# INLINE uniqMaxPoetical2GNL #-}
  
-- | Prints @n@ (given as the first 'Int' argument) maximum elements with respect to the several norms (their quantity is the second 'Int' argument) starting 
-- from the right to the left. The last norm is the first element in the 'V.Vector' of norms (@[Int] -> Int@). 
-- Uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
uniqNPoetical2GN :: Preapp -> Int -> Int -> V.Vector ([Int] -> Int) -> (String -> [Int]) -> String -> IO ()
uniqNPoetical2GN rr n k vN g xs
 | n <= 0 = return ()
 | otherwise = do
   let v = uniquenessVariants2GNP (get1m rr) (get2m rr) vN g xs
   if compare (V.length v) n == LT
     then V.mapM_ (\x -> putStr ((filter (not . isPunctuation) . lastFrom3 $ x)) >> putStrLn "" ) v
     else (uniqInMaxPoeticalN k vN (U v) >>= uniqNPoeticalN (n - 1) k vN)

-- | Variant of 'uniqNPoetical2GN' where all the elements in the norms 'V.Vector' are used as norms from right to left.
-- Uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
uniqNPoetical2GNL :: Preapp -> Int -> V.Vector ([Int] -> Int) -> (String -> [Int]) -> String -> IO ()
uniqNPoetical2GNL rr n vN = uniqNPoetical2GN rr n (V.length vN) vN
{-# INLINE uniqNPoetical2GNL #-}
 
-- | Generalized variant of the 'uniqNPoeticalG' with usage of the several norms, but prints its output on the same line. 
-- Uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
uniqNPoetical2GNLine :: Preapp -> Int -> Int -> V.Vector ([Int] -> Int) -> (String -> [Int]) -> String -> IO ()
uniqNPoetical2GNLine rr n k vN g xs
 | n <= 0 = putStrLn ""
 | otherwise = do
   let v = uniquenessVariants2GNP (get1m rr) (get2m rr) vN g xs
   if compare (V.length v) n == LT
     then V.mapM_ (\x -> putStr ((filter (not . isPunctuation) . lastFrom3 $ x)) >> putStr " " ) v >> putStrLn ""
     else (uniqInMaxPoeticalNLine k vN (U v) >>= uniqNPoeticalNLine (n - 1) k vN)

-- | Variant of 'uniqNPoetical2GNLine' where all the elements in the norms 'V.Vector' are used as norms from right to left.
-- Uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
uniqNPoetical2GNLineL :: Preapp -> Int -> V.Vector ([Int] -> Int) -> (String -> [Int]) -> String -> IO ()
uniqNPoetical2GNLineL rr n vN = uniqNPoetical2GNLine rr n (V.length vN) vN
{-# INLINE uniqNPoetical2GNLineL #-}
 
-- | Prints @n@ (given as the first 'Int' argument) maximum elements with respect to the several norms (their quantity is the second 'Int' argument) starting 
-- from the right to the left. The last norm is the first element in the 'V.Vector' of norms (@[Int] -> Int@). Contrary to its pair function 
-- 'uniqNPoetical2GN' returns then the rest of the given 'V.Vector' 'Uniqueness' after filtering the printed elements 'String'.
-- Uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- To specify whether the function returns also data suitable for usage with other functions or just usable mostly for printing it uses also a before 'String' 
-- argument also 'UniqG' one with the 'U' data constructor corresponding to the printing (mostly) and 'UL' to possible reusage of data.
uniqNPoetical2VGN :: Preapp -> Int -> Int -> V.Vector ([Int] -> Int) -> (String -> [Int]) -> UniqG -> String -> IO UniqG
uniqNPoetical2VGN rr n k vN g y xs
 | n <= 0 = if isU y then return (U V.empty) else return (UL ([],V.empty))
 | otherwise = do
   let v = uniquenessVariants2GNP (get1m rr) (get2m rr) vN g xs
   if compare (V.length v) n == LT 
     then if isU y then return (U v) else return (UL ([],v)) 
     else if isU y then uniqNPoeticalVN n k vN (U v) else uniqNPoeticalVN n k vN (UL ([],v))

-- | Variant of 'uniqNPoetical2VGN' where all the elements in the norms 'V.Vector' are used as norms from right to left.
-- Uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- To specify whether the function returns also data suitable for usage with other functions or just usable mostly for printing it uses also a before 'String' 
-- argument also 'UniqG' one with the 'U' data constructor corresponding to the printing (mostly) and 'UL' to possible reusage of data.
uniqNPoetical2VGNL :: Preapp -> Int -> V.Vector ([Int] -> Int) -> (String -> [Int]) -> UniqG -> String -> IO UniqG
uniqNPoetical2VGNL rr n vN = uniqNPoetical2VGN rr n (V.length vN) vN
{-# INLINE uniqNPoetical2VGNL #-}
 
-- | Variant of the 'uniqNPoetical2GN', which uses as a function 'uniquenessPeriods2' with the first argument equal to the first 'Int' argument.
-- Uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation). The second argument is a function that 
-- converts a 'String' of the text into the 'V.Vector' of sound representations for that language. 
uniqNPoeticalUGN_ :: [String] -> (String -> V.Vector String) -> Preapp -> Int -> Int -> Int -> V.Vector ([Int] -> Int) -> String -> IO ()
uniqNPoeticalUGN_ whspss h rr x n k vN = uniqNPoetical2GN rr n k vN (uniquenessPeriods2 whspss h x)
{-# INLINE uniqNPoeticalUGN_ #-}

-- | Variant of 'uniqNPoeticalUGN_' where all the elements in the norms 'V.Vector' are used as norms from right to left.
-- Uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation). The second argument is a function that 
-- converts a 'String' of the text into the 'V.Vector' of sound representations for that language. 
uniqNPoeticalUGNL_ :: [String] -> (String -> V.Vector String) -> Preapp -> Int -> Int -> V.Vector ([Int] -> Int) -> String -> IO ()
uniqNPoeticalUGNL_ whspss h rr x n vN = uniqNPoetical2GNL rr n vN (uniquenessPeriods2 whspss h x)
{-# INLINE uniqNPoeticalUGNL_ #-}

-- | Variant of the 'uniqNPoetical2VGN', which uses as a function 'uniquenessPeriods2' with the first argument equal to the first 'Int' argument.
-- Uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- To specify whether the function returns also data suitable for usage with other functions or just usable mostly for printing it uses also a before 'String' 
-- argument also 'UniqG' one with the 'U' data constructor corresponding to the printing (mostly) and 'UL' to possible reusage of data.
-- The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation). The second argument is a function that 
-- converts a 'String' of the text into the 'V.Vector' of sound representations for that language. 
uniqNPoeticalUGN :: [String] -> (String -> V.Vector String) -> Preapp -> Int -> Int -> Int -> V.Vector ([Int] -> Int) -> UniqG -> String -> IO UniqG
uniqNPoeticalUGN whspss h rr x n k vN = uniqNPoetical2VGN rr n k vN (uniquenessPeriods2 whspss h x)
{-# INLINE uniqNPoeticalUGN #-}

-- | Variant of 'uniqNPoeticalUGN' where all the elements in the norms 'V.Vector' are used as norms from right to left.
-- Uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- To specify whether the function returns also data suitable for usage with other functions or just usable mostly for printing it uses also a before 'String' 
-- argument also 'UniqG' one with the 'U' data constructor corresponding to the printing (mostly) and 'UL' to possible reusage of data.
-- The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation). The second argument is a function that 
-- converts a 'String' of the text into the 'V.Vector' of sound representations for that language. 
uniqNPoeticalUGNL :: [String] -> (String -> V.Vector String) -> Preapp -> Int -> Int -> V.Vector ([Int] -> Int) -> UniqG -> String -> IO UniqG
uniqNPoeticalUGNL whspss h rr x n vN = uniqNPoetical2VGN rr n (V.length vN) vN (uniquenessPeriods2 whspss h x)
{-# INLINE uniqNPoeticalUGNL #-}

-- | Variant of the 'uniqNPoeticalUGN_', which uses as a single norm 'norm51'.
-- Uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation). The second argument is a function that 
-- converts a 'String' of the text into the 'V.Vector' of sound representations for that language. 
uniqNPoeticalUGN51_ :: [String] -> (String -> V.Vector String) -> Preapp -> Int -> Int -> String -> IO ()
uniqNPoeticalUGN51_ whspss h rr x n = uniqNPoeticalUGN_ whspss h rr x n 1 (V.singleton norm51)
{-# INLINE uniqNPoeticalUGN51_ #-}

-- | Variant of the 'uniqNPoeticalUGN', which uses as a single norm 'norm51'.
-- Uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- To specify whether the function returns also data suitable for usage with other functions or just usable mostly for printing it uses also a before 'String' 
-- argument also 'UniqG' one with the 'U' data constructor corresponding to the printing (mostly) and 'UL' to possible reusage of data.
-- The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation). The second argument is a function that 
-- converts a 'String' of the text into the 'V.Vector' of sound representations for that language. 
uniqNPoeticalUGN51 :: [String] -> (String -> V.Vector String) -> Preapp -> Int -> Int -> UniqG -> String -> IO UniqG
uniqNPoeticalUGN51 whspss h rr x n = uniqNPoeticalUGN whspss h rr x n 1 (V.singleton norm51)
{-# INLINE uniqNPoeticalUGN51 #-}
  
