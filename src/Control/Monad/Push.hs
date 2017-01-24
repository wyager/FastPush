-- Hybrid ReaderT/StateT style
-- 100M Ints in ~3.1 seconds
-- 2.95G allocated. Seems to match minimal ST loop

{-# LANGUAGE TypeApplications, RankNTypes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveFunctor, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

module Control.Monad.Push where

import Control.Monad.ST (ST, runST)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Generic.Mutable as VGM
import Data.STRef.Strict (STRef, readSTRef, writeSTRef, newSTRef)
import Control.Monad.Push.Class (MonadPush, push)

-- | The internal return type of a push action.
-- The Int value is the new vector used length.
data Res a = Res !Int
                 !a deriving Functor

-- | A monad  that lets you push things onto a stack.
newtype Push v p a = Push (forall s . Int -> (STRef s (v s p)) -> (ST s (Res a))) deriving (Functor)

instance Applicative (Push v p) where
    {-# INLINE pure #-}
    pure a = Push $ \u _ -> (return (Res u a))
    {-# INLINE (<*>) #-}
    (Push f) <*> (Push g) = Push $ \u v -> f u v >>= (\(Res u' o1) -> g u' v >>= (\(Res u'' o2) -> return (Res u'' (o1 o2))))

instance Monad (Push v p) where
    {-# INLINE return #-}
    return = pure
    {-# INLINE (>>=) #-}
    Push g >>= f = Push $ \u v -> ((g u v) >>= (\(Res u' x) -> let Push h = f x in h u' v))
    {-# INLINE (>>) #-}
    Push g >> Push h = Push $ \u v -> g u v >>= (\(Res u' _) -> h u' v)

instance VGM.MVector v p => MonadPush p (Push v p) where
    {-# INLINE push #-}
    push a = Push $ \used vec' -> do
        vec <- readSTRef vec'
        if (VGM.length vec == used) 
            then do
                bigger <- VGM.grow vec used -- Double the length
                VGM.write bigger used a
                writeSTRef vec' bigger
            else do
                VGM.write vec used a
        return $ Res (used+1) ()

-- | Run the Push monad. Get the return value and the output stack.
{-# INLINE runPush #-}
runPush :: VG.Vector v p => Push (VG.Mutable v) p a -> (a, v p)
runPush (Push action) = runST $ do
    initial <- VGM.new 1
    vecRef <- newSTRef initial
    (Res used out) <- action 0 vecRef
    final <- readSTRef vecRef
    vec <- VG.freeze (VGM.slice 0 used final)
    return (out, vec)

-- | Specialized to Unboxed vectors.
{-# INLINE runPushU #-}
runPushU :: forall p a . VU.Unbox p => Push (VU.MVector) p a -> (a, VU.Vector p)
runPushU = runPush
-- | Specialized to standard Boxed vectors.
{-# INLINE runPushB #-}
runPushB :: forall p a . Push (V.MVector) p a -> (a, V.Vector p)
runPushB = runPush
-- | Specialized to Storable vectors.
{-# INLINE runPushS #-}
runPushS :: forall p a . VS.Storable p => Push (VS.MVector) p a -> (a, VS.Vector p)
runPushS = runPush
