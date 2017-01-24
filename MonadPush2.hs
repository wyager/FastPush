-- ReaderT style
-- 100M in ~3.6 seconds
-- 4.55G allocated

{-# LANGUAGE TypeApplications, RankNTypes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveFunctor, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

module MonadPush2 where

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

class Monad m => MonadPush a m where
    push :: a -> m ()


newtype Push v p a = Push (forall s . (STRef s Int) -> (STRef s (v s p)) -> (ST s a)) deriving (Functor)

instance Applicative (Push v p) where
    {-# INLINEABLE pure #-}
    pure a = Push $ \u v -> (return a)
    {-# INLINEABLE (<*>) #-}
    (Push f) <*> (Push g) = Push $ \u v -> f u v <*> g u v

instance Monad (Push v p) where
    {-# INLINEABLE return #-}
    return = pure
    {-# INLINEABLE (>>=) #-}
    Push g >>= f = Push $ \u v -> ((g u v) >>= (\x -> let Push h = f x in h u v))
    {-# INLINEABLE (>>) #-}
    Push g >> Push h = Push $ \u v -> g u v >> h u v

instance VGM.MVector v p => MonadPush p (Push v p) where
    {-# INLINABLE push #-}
    push a = Push $ \used' vec' -> do
        used <- readSTRef used'
        vec <- readSTRef vec'
        if (VGM.length vec == used) 
            then do
                bigger <- VGM.grow vec used -- Double the length
                VGM.write bigger used a
                writeSTRef vec' bigger
            else do
                VGM.write vec used a
        writeSTRef used' (used + 1)

{-# SPECIALIZE runPush :: Push (VU.MVector) Int a -> (a, VU.Vector Int) #-}
runPush :: VG.Vector v p => Push (VG.Mutable v) p a -> (a, v p)
runPush (Push action) = runST $ do
    initial <- VGM.new 1
    vecRef <- newSTRef initial
    usedRef <- newSTRef 0
    out <- action usedRef vecRef
    final <- readSTRef vecRef
    used <- readSTRef usedRef
    vec <- VG.freeze (VGM.slice 0 used final)
    return (out, vec)


runPushU :: forall p a . VU.Unbox p => Push (VU.MVector) p a -> (a, VU.Vector p)
runPushU = runPush

runPushB :: forall p a . Push (V.MVector) p a -> (a, V.Vector p)
runPushB = runPush

runPushS :: forall p a . VS.Storable p => Push (VS.MVector) p a -> (a, VS.Vector p)
runPushS = runPush

test :: forall m . MonadPush Int m => m ()
test = go 100000000
    where
    go :: Int -> m ()
    go 0 = push (5 :: Int)
    go n = push (5 :: Int) >> go (n-1)

-- runU :: 
ioTest :: IO ()
ioTest = print $ VG.length $ snd $ runPushU @Int $ test

