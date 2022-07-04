{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Control.Functor.Linear.Internal.Identity
  ( --  IdentityT monad transformer
    IdentityT (..),
    runIdentityT,
    mapIdentityT,
  )
where

import Control.Functor.Linear.Internal.Class
import Control.Functor.Linear.Internal.Instances ()
import Control.Functor.Linear.Internal.MonadTrans
import qualified Control.Monad as NonLinear ()
import qualified Data.Functor.Linear.Internal.Applicative as Data
import qualified Data.Functor.Linear.Internal.Functor as Data
import Prelude.Linear.Internal (($), (.))

-- # Linear IdentityT
-------------------------------------------------------------------------------

-- | A linear identity monad transformer.
newtype IdentityT m a = IdentityT (m a)

runIdentityT :: IdentityT m a %1 -> m a
runIdentityT (IdentityT ma) = ma

mapIdentityT :: (m a %1 -> n b) %1 -> IdentityT m a %1 -> IdentityT n b
mapIdentityT f (IdentityT ma) = IdentityT $ f ma

instance Data.Functor m => Data.Functor (IdentityT m) where
  fmap f = mapIdentityT $ Data.fmap f

instance Functor m => Functor (IdentityT m) where
  fmap f = mapIdentityT $ fmap f

instance Data.Applicative m => Data.Applicative (IdentityT m) where
  pure x = IdentityT $ Data.pure x
  IdentityT f <*> IdentityT x = IdentityT $ f Data.<*> x

instance Applicative m => Applicative (IdentityT m) where
  pure x = IdentityT $ pure x
  IdentityT f <*> IdentityT x = IdentityT $ f <*> x

instance Monad m => Monad (IdentityT m) where
 IdentityT x >>= f = IdentityT $ x >>= runIdentityT . f

instance MonadTrans IdentityT where
  lift = IdentityT

instance MonadTransUnlift IdentityT where
  liftWithUnlift = liftWith

instance MonadTransControl IdentityT where
  type StT IdentityT a = a
  liftWith f = IdentityT $ f runIdentityT
  restoreT = IdentityT
