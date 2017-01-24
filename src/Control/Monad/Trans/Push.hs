-- Hybrid ReaderT/StateT style over STT
-- 100M Ints in ~4 seconds over IO
-- Seems to optimize away the inner monad pretty well when feasible

{-# LANGUAGE TypeApplications, RankNTypes, MultiParamTypeClasses, 
    GeneralizedNewtypeDeriving, DeriveFunctor, FlexibleInstances, 
    FlexibleContexts, ScopedTypeVariables, StandaloneDeriving, UnboxedTuples #-}

module Control.Monad.Trans.Push where

import Control.Monad.ST.Trans (runST, STRef, readSTRef, writeSTRef, newSTRef)
import Control.Monad.ST.Trans.Internal (STT(STT), STTRet(STTRet))
import GHC.ST (ST(ST))
import Control.Monad.Identity (Identity, runIdentity)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Control.Monad.Trans.Class as Trans
import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.STRef.Strict as Ref
import qualified Data.Foldable as Foldable
import Control.Monad.Push.Class (MonadPush, push)

-- | The internal return type of a push action.
-- The Int value is the new vector used length.
data Res a = Res !Int
                 !a deriving Functor

-- | A monad transformer that lets you push things onto a stack.
-- This is probably super unsafe; see the docs for Control.Monad.ST.Trans.
newtype PushT v p m a = PushT (forall s . Int -> (STRef s (v s p)) -> (STT s m (Res a)))

deriving instance Functor m => Functor (PushT v p m)

instance Monad m => Applicative (PushT v p m) where
    {-# INLINE pure #-}
    pure a = PushT $ \u v -> (return (Res u a))
    {-# INLINE (<*>) #-}
    (PushT f) <*> (PushT g) = PushT $ \u v -> f u v >>= (\(Res u' o1) -> g u' v >>= (\(Res u'' o2) -> return (Res u'' (o1 o2))))

instance Monad m => Monad (PushT v p m) where
    {-# INLINE return #-}
    return = pure
    {-# INLINE (>>=) #-}
    PushT g >>= f = PushT $ \u v -> ((g u v) >>= (\(Res u' x) -> let PushT h = f x in h u' v))
    {-# INLINE (>>) #-}
    PushT g >> PushT h = PushT $ \u v -> g u v >>= (\(Res u' _) -> h u' v)

-- | This seems *highly* questionable, but appears to get the job done.
{-# INLINE liftST #-}
liftST :: Applicative m => ST s a -> STT s m a
liftST (ST f) = STT (\s -> let (# s', a #) = f s in pure (STTRet s' a))

instance (Monad m, VGM.MVector v p) => MonadPush p (PushT v p m) where
    {-# INLINE push #-}
    push a = PushT $ \used vec' -> do
        vec <- readSTRef vec'
        if (VGM.length vec == used) 
            then do
                bigger <- liftST $ VGM.grow vec used -- Double the length
                liftST $ VGM.write bigger used a
                writeSTRef vec' bigger
            else do
                liftST $ VGM.write vec used a
        return $ Res (used+1) ()

-- | Run the monad transformer.
{-# INLINE runPushT #-}
runPushT :: (Monad m, VG.Vector v p) => PushT (VG.Mutable v) p m a -> m (a, v p)
runPushT (PushT action) = runST $ do
    initial <- liftST $ VGM.new 1
    vecRef <- newSTRef initial
    (Res used out) <- action 0 vecRef
    final <- readSTRef vecRef
    vec <- liftST $ VG.freeze (VGM.slice 0 used final)
    return (out, vec)

-- | Specialized to Unboxed vectors.
{-# INLINE runPushTU #-}
runPushTU :: forall p a m . (VU.Unbox p, Monad m) => PushT (VU.MVector) p m a -> m (a, VU.Vector p)
runPushTU = runPushT
-- | Specialized to standard Boxed vectors.
{-# INLINE runPushTB #-}
runPushTB :: forall p a m . Monad m => PushT (V.MVector) p m a -> m (a, V.Vector p)
-- | Specialized to Storable vectors.
runPushTB = runPushT
{-# INLINE runPushTS #-}
runPushTS :: forall p a m . (VS.Storable p, Monad m) => PushT (VS.MVector) p m a -> m (a, VS.Vector p)
runPushTS = runPushT
