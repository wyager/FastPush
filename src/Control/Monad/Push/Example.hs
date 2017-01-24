{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Control.Monad.Push.Example where

import qualified Control.Monad.Push as P
import qualified Control.Monad.Trans.Push as TP
import Control.Monad.Push.Class (MonadPush, push)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU

-- | A simple test routine, generic over choice of MonadPush instance.
test :: MonadPush Int m => m ()
test = go (100000000 :: Int)
    where
    go 0 = push (5 :: Int)
    go n = push (5 :: Int) >> go (n-1)

-- | Using the pure Push monad
testMonad :: IO ()
testMonad = let (_, vec :: VU.Vector Int) = P.runPushU test in print (VG.length vec)

-- | Using PushT over IO
testTransformer :: IO ()
testTransformer = do
    (_, vec :: VU.Vector Int) <- TP.runPushTU test
    print (VG.length vec)