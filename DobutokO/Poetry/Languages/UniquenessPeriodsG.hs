-- |
-- Module      :  DobutokO.Poetry.Languages.UniquenessPeriodsG
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to order the 7 or less words (or their concatenations) 
-- to obtain (to some extent) suitable for poetry or music text. This module 
-- provides a functionality to define more complex uniquenessPeriods functions.
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

{-# LANGUAGE CPP #-}

module DobutokO.Poetry.Languages.UniquenessPeriodsG where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif

import qualified Data.Vector as V
import Data.List ((\\),nubBy)
import String.UniquenessPeriodsG

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

-- | More complicated and longer variant of the 'uniquenessPeriods' that takes into account the second order structure of uniqueness with 'uniquenessP2' and 
-- can be therefore more fruitful (probably, it is a hypothesis itself that is needed to be tested). Is provided here as an example of the more complex 
-- \"uniqueness function\". Uses both 'uniqueness2' and 'uniqueness2n' inside and is actually their composition with some (hopefully, natural) parameter functions.
-- The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation). The second argument is a function that 
-- converts a 'String' of the text into the 'V.Vector' of sound representations for that language. 
uniquenessPeriods2 :: [String] -> (String -> V.Vector String) -> Int -> String -> [Int]
uniquenessPeriods2 whspss g x = uniqueness2n (show7snc whspss) (length) x . uniqueness2 (show7s6 whspss g) (uniquenessP2)

-- | Parameterized way to prepare the result that can be used with 'uniqueness2n'. 
uniqueness2 :: (String -> [[String]]) -> ([[String]] -> [[String]]) -> String -> ([[String]],[String])
uniqueness2 f h xs 
 | null xs = ([],[])
 | otherwise = 
    let ys = f xs
        y2s = mconcat . h $ ys in (ys,y2s)

-- | Being given two functions as parameters uses them to create a longer list of 'Int' than just application of only one of them. Besides, it can take into 
-- account the possible 0 and to create a non-negative list of 'Int' that can be used e. g. by 'DobutokO.Poetry.Norms.splitNorm'.
uniqueness2n :: ([String] -> [Int]) -> ([String] -> Int) -> Int -> ([[String]], [String]) -> [Int]
uniqueness2n h f2 x (ys,y2s) 
 | x == 0 = fmap f2 ys ++ (0:h y2s)
 | otherwise = fmap f2 ys ++ h y2s

-- | Approximates the higher order computational scheme of the 'uniqueness2n' function considering the latter one as the second order computational scheme. Because 
-- of the possible concatenation and other not phonetic coversions it cannot be considered the exact one, but in some cases can give more information about phonetic 
-- \"uniqueness periods\" structure, so is provided here for exploration.
uniqueness2nG :: [String] -> (String -> V.Vector String) -> Int -> ([String] -> [Int]) -> ([String] -> Int) -> Int -> ([[String]], [String]) -> [Int] 
uniqueness2nG whspss g n h f2 x (ys,y2s)
 | compare n 2 == LT = error "DobutokO.Poetry.Languages.UniquenessPeriodsG.uniqueness2nG: undefined for that third argument. "
 | n == 2 = uniqueness2n h f2 x (ys,y2s)
 | x == 0 = fmap f2 ys ++ (0:(uniqueness2nG whspss g (n - 1) h f2 0 . uniqueness2 (show7s6 whspss g) (uniquenessP2) . mconcat $ y2s))
 | otherwise = fmap f2 ys ++ (uniqueness2nG whspss g (n - 1) h f2 x . uniqueness2 (show7s6 whspss g) (uniquenessP2) . mconcat $ y2s)

-- | Approximates the higher order computational scheme of the 'uniquenessPeriods2' function considering the latter one as the second order computational 
-- scheme. Because of the possible concatenation and other not phonetic coversions it cannot be considered the exact one, but in some cases can give more 
-- information about phonetic \"uniqueness periods\" structure, so is provided here for exploration.
uniquenessPeriodsN :: [String] -> (String -> V.Vector String) -> Int -> Int -> String -> [Int]
uniquenessPeriodsN whspss g n x = uniqueness2nG whspss g n (show7snc whspss) (length) x . uniqueness2 (show7s6 whspss g) (uniquenessP2)

-- | Filters a given arguments so that each element 'String' in the result is filtered from the element, which is doubled the first in the next 'String' 
-- (usually, it equals to the head of it, if used as expected). Can be interpreted as a preparation to the second application of the 'uniquenessPeriods' 
-- function because it removes the elements that splitted the input into lists and can be seen as a second deeper (so, probably less significant) factor 
-- of the uniqueness phonetic structure. 
uniquenessP2 :: [[String]] -> [[String]]
uniquenessP2 (yss:ysss) 
  | null ysss = [yss]
  | otherwise = if length yss == 1 then uniquenessP2 ysss else (yss \\ [mconcat . take 1 . mconcat . take 1 $ ysss]):uniquenessP2 ysss
uniquenessP2 _ = []
