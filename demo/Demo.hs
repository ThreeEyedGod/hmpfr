{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MagicHash #-}
module Main where

import qualified Data.Number.MPFR as M --import functions
import Data.Number.MPFR.Instances.Up ()-- import instances

import qualified Data.Number.MPFR.Mutable as MM

import Control.Monad.ST(runST)
import Data.Number.MPFR (RoundMode)
import GHC.Num.Integer (integerLogBase, integerLog2)
import Data.Char (intToDigit)
import Data.Digits (digits)
import Data.Bits
import Data.List (unfoldr)
import Debug.Trace
import Data.Primitive.Types (sizeOf, sizeOfType)
import Data.Word


-- compute the sqrt
sqrtInt     :: Int -> M.MPFR
sqrtInt n = M.sqrt M.Near 1000 (M.fromInt M.Near 32 n)

sqrtInteger :: Integer -> M.MPFR
sqrtInteger n = M.sqrtw M.Near 1000 wrd where
        wrd = M.toWord M.Near uMPFR  
        uMPFR = M.fromIntegerA M.Near 1000 n

sqrtDouble :: Double -> M.MPFR
sqrtDouble d = M.sqrt M.Near 1000 (M.fromDouble M.Near 1000 d)

sqrtBigFloat :: Integer -> M.MPFR
sqrtBigFloat i =  M.sqrt M.Near 1000 $ M.fromIntegerA M.Near 10000 i 

decompose :: Integer -> (Word, Int)
decompose i
  | i < 0 = error "Input must be a non-negative integer"
  | otherwise = (fromIntegral x, y)
  where
    (y, x) = integerLog2 i
    integerLog2 n = go n 0
      where
        go 0 acc = (acc - 1, 0) -- Handle the case for 0
        go m acc =
          if m `mod` 2 == 0
            then go (m `div` 2) (acc + 1)
            else (acc, m)

integerToString :: Integer -> String
integerToString i = intsToDigits (digits 10 (fromIntegral i)) 

intsToDigits :: [Int] -> String
intsToDigits = map intToDigit


-- compute the sum from 1 to n with precision of p bits rounded to Near
s1     :: M.Precision -> Int -> M.MPFR
s1 p n = s1' 1 0
    where s1' k acc | k <= n = s1' (succ k) (M.add M.Near p acc (M.fromInt M.Near 32 k))
                    | otherwise = acc

-- or the same using addi + foldl instead of add
s2    :: M.Precision -> Int -> M.MPFR
s2 p  = foldl (M.addi M.Near p) 0 . enumFromTo 1

-- or the same as s1 except with foldl
s3 :: M.Precision -> Int -> M.MPFR
s3 p = foldl ((. M.fromInt M.Up p) . (+)) M.zero . enumFromTo 1

-- or idiomatically using the MPFR Num instance
-- note that this version is a lot slower than previous three
-- guess why :)
s4 :: M.Precision -> Int -> M.MPFR
s4 p = sum . map (M.fromInt M.Up p) . enumFromTo 1

-- or with mutable MPFR
s5 :: M.Precision -> Int -> M.MPFR
s5 p n = runST $ go n =<< MM.unsafeThaw (M.fromInt M.Near p 0)
    where go 0 acc = MM.unsafeFreeze acc
          go m acc = MM.addi acc acc m M.Near >> go (m-1) acc

-- or, if you're feeling haskelly
s6 :: M.Precision -> Int -> M.MPFR
s6 p n = runST $ MM.unsafeThaw (M.fromInt M.Near p 0) >>=
         \acc -> mapM_ (flip (MM.addi acc acc) M.Near) [1..n] >>
         MM.unsafeFreeze acc

-- sum up first n terms of a Taylor series for e with precision p
e1     :: M.Precision -> Int -> M.MPFR
e1 p n = e' 1 1 1
    where e' k acc acc' | k == n+1 = acc
                        | otherwise= e' (succ k) (M.add M.Up p acc acc'') acc''
                        where acc'' = M.divi M.Up p acc' k

-- or using mutable MPFR's
e2     :: M.Precision -> Int -> M.MPFR
e2 p n = let oneE2 = M.fromInt M.Near p 1
         in runST $ do acc <- MM.unsafeThaw one
                       acc' <- MM.thaw oneE2
                       mapM_ ((>> MM.add acc acc acc' M.Up)
                              . flip (MM.divi acc' acc') M.Up) [1..n]
                       MM.unsafeFreeze acc

testRandom :: IO ()
testRandom =
    do
    let rsP = M.newRandomStatePointer
    print $ M.urandomb rsP 1000
    print $ M.urandomb rsP 1000
    print $ M.urandomb rsP 1000
    print $ M.urandomb rsP 1000

main :: IO ()
main = do --print $ sqrtInt (2^30)
          --print $ sqrtInteger (2^63) -- integer maxes out at 2^64
          --print $ sqrtDouble 123456789012345.62030030030030303030
          --let testInteger1 = (2^63 - 1) :: Integer
          --print $ sqrtBigFloat testInteger1
          let testInteger2 = (123 * 10^605) :: Integer 
          print $ sqrtBigFloat testInteger2


          -- print $ integerLogBase 10 (toInteger $ M.toWord M.Near (sqrtBigFloat testInteger))
        --   print $ s1 1000 100000
        --   print $ s6 1000 100000
        --   print $ e1 1000 100000
        --   print $ e2 1000 100000
        --   testRandom
        --   let c1 = one
        --   putStrLn $ "exp 1 = " ++ (M.toStringExp 10000 $ M.exp M.Up 100 c1)
        --   putStrLn $ "exp -1 = " ++ (M.toStringExp 10000 $ M.exp M.Up 100 (M.neg M.Up 100 c1))
    
one :: M.MPFR
one = 1 :: M.MPFR
