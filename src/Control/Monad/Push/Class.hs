{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

module Control.Monad.Push.Class where

class Monad m => MonadPush a m where
    -- | Push an item onto the stack
    push :: a -> m ()
