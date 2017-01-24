-- StateT style
-- 100M in ~3.5 seconds
-- 9.4G allocated

{-# LANGUAGE BangPatterns, TypeApplications, RankNTypes, MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveFunctor, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

module MonadPush3 where

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

data Res v s p a = Res !Int 
                       !(v s p) 
                       !a deriving Functor

data In v s p = In !Int !(v s p)

newtype Push v p a = Push (forall s . In v s p -> ST s (Res v s p a)) deriving (Functor)

instance Applicative (Push v p) where
    {-# INLINEABLE pure #-}
    pure a = Push $ \(In u v) -> return (Res u v a)
    {-# INLINEABLE (<*>) #-}
    (Push f) <*> (Push g) = Push $ \(In u v) -> (f (In u v) >>= \(Res u' v' h) -> (g (In u' v') >>= \(Res u'' v'' h') -> return (Res u'' v'' (h h'))))

instance Monad (Push v p) where
    {-# INLINEABLE return #-}
    return = pure
    {-# INLINEABLE (>>=) #-}
    Push g >>= f = Push $ \(In u v) -> ((g (In u v)) >>= (\(Res u' v' h) -> let Push i = f h in i (In u' v')))
    {-# INLINEABLE (>>) #-}
    Push g >> Push h = Push $ \(In u v) -> (g (In u v) >>= \(Res u' v' _) -> h (In u' v'))

instance VGM.MVector v p => MonadPush p (Push v p) where
    {-# INLINABLE push #-}
    push a = Push $ \(In used vec) -> do
        if (VGM.length vec == used) 
            then do
                bigger <- VGM.grow vec used -- Double the length
                VGM.write bigger used a
                return $ Res (used+1) bigger ()
            else do
                VGM.write vec used a
                return $ Res (used+1) vec ()

runPush :: VG.Vector v p => Push (VG.Mutable v) p a -> (a, v p)
runPush (Push action) = runST $ do
    initial <- VGM.new 1
    Res used final out <- action (In 0 initial)
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

-- -- runU :: 
ioTest :: IO ()
ioTest = print $ VG.length $ snd $ runPushU @Int $ test

