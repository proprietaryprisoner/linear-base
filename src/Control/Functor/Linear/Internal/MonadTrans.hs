{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Control.Functor.Linear.Internal.MonadTrans
  ( MonadTrans (..),
    MonadTransUnlift (..),
    MonadTransControl (..),
  )
where

import Control.Functor.Linear.Internal.Class
import Data.Kind

class (forall m. Monad m => Monad (t m)) => MonadTrans t where
  lift :: Monad m => m a %1 -> t m a

class MonadTrans t => MonadTransUnlift t where
  liftWithUnlift :: Monad m => ((forall x. t m x %1 -> m x) %1 -> m a) %1 -> t m a

class MonadTrans t => MonadTransControl t where
  type StT t a :: Type
  liftWith :: Monad m => ((forall x. t m x %1 -> m (StT t x)) %1 -> m a) %1 -> t m a
  restoreT :: Monad m => m (StT t a) %1 -> t m a
