-- No monad except ST
-- 100M in 2.5-3 seconds
-- 2.95G allocated

{-# LANGUAGE BangPatterns, TypeApplications, RankNTypes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveFunctor, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

module BaselinePush where

import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.ST (ST, runST)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Control.Monad.Trans.Class as Trans
import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Generic.Mutable as VGM
import Data.STRef.Strict (STRef, readSTRef, writeSTRef, newSTRef)
import qualified Data.STRef.Strict as Ref
import qualified Data.Foldable as Foldable


thing :: ST s (VU.Vector Int)
thing = do
        initial <- VGM.new (1 :: Int)
        loop (100000000 :: Int) (0 :: Int) initial
    where
    loop !(0 :: Int) !(used :: Int) !vec = VG.freeze (VGM.slice 0 used vec)
    loop !n          !(used :: Int) !vec = if (VGM.length vec == used) 
        then do
            bigger <- VGM.grow vec used -- Double the length
            VGM.unsafeWrite bigger used 5
            loop (n-1) (used+1) bigger
        else do
            VGM.unsafeWrite vec used 5
            loop (n-1) (used+1) vec


ioTest :: IO ()
ioTest = print $ VG.length $ runST thing
