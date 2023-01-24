{-# LANGUAGE NoImplicitPrelude #-}
module Reverse
  ( myReverse
  , betterReverse
  , vectorReverse
  , uvectorReverse
  , svectorReverse
  ) where

import RIO
import qualified RIO.Vector as V
import qualified RIO.Vector.Boxed as VB
import qualified RIO.Vector.Storable as VS
import qualified RIO.Vector.Unboxed as VU
import qualified Data.Vector.Generic.Mutable as VM

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

betterReverse :: [a] -> [a]
betterReverse =
    loop []
  where
    loop res [] = res
    loop res (x:xs) = loop (x:res) xs

vectorReverseGeneric
  :: V.Vector v a
  => [a]
  -> v a
vectorReverseGeneric input = V.create $ do
  let len = length input
  v <- VM.new len
  let loop [] idx = assert (idx == -1) (return v)
      loop (x:xs) idx = do
        VM.unsafeWrite v idx x
        loop xs (idx - 1)
  loop input (len - 1)
{-# INLINEABLE vectorReverseGeneric #-}

vectorReverse :: [a] -> [a]
vectorReverse = VB.toList . vectorReverseGeneric
{-# INLINE vectorReverse #-}

svectorReverse :: VS.Storable a => [a] -> [a]
svectorReverse = VS.toList . vectorReverseGeneric
{-# INLINE svectorReverse #-}

uvectorReverse :: VU.Unbox a => [a] -> [a]
uvectorReverse = VU.toList . vectorReverseGeneric
{-# INLINE uvectorReverse #-}