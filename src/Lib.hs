{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import Control.Arrow (left)
import Control.Monad ((<=<))
import Control.Monad.Except
import Control.Monad.Trans
import Data.OpenUnion hiding ((:<))
import qualified Data.OpenUnion as OU
-- import Data.Type.List
import Data.Typeable (Typeable, Proxy (Proxy))
import Data.Void (Void)
import GHC.Prim (Constraint, coerce)

class (MonadError e m, MonadError e' m') => MonadErrorMap e m e' m' | e m' -> m where
    emap :: (e -> e') -> m a -> m' a

instance (Monad m) => MonadErrorMap e (ExceptT e m) e' (ExceptT e' m) where
    emap = withExceptT

-- instance (Monad m, MonadError e' (t m), MonadTrans t) => MonadErrorMap e (ExceptT e m) e' (t m) where
--     emap f = go <=< lift . runExceptT where
--         go :: forall m' a. (MonadError e' m') => (Either e a) -> m' a
--         go = either (throwError . f) pure

instance MonadErrorMap e (Either e) e' (Either e') where
    emap = left

type family (:∈) (x :: *) (ys :: [*]) :: Constraint where
    x :∈ '[y] = (x ~ y)
    x :∈ (x ': ys) = ()
    x :∈ (y ': ys) = x :∈ ys

type family (:<) (s :: [*]) (s' :: [*]) :: Constraint where
    '[]      :< s'        = ()
    (a ': s) :< s'        = (s :< s', a :∈ s', Typeable a)

instance (s :< s') => (OU.:<) s s'

throw :: (MonadError (Union s) m, e :∈ s, Typeable e) => e -> m a
throw = throwError . liftUnion

rethrow :: (MonadErrorMap (Union s) m (Union s') m', Typeable e, Typeable e', e :∈ s, (s :\ e) :< s', e' :∈ s') => (e -> e') -> m a -> m' a
rethrow f = emap f' where
    f' s = case restrict s of
        Left  u -> reUnion u
        Right e -> liftUnion $ f e

singleError :: (MonadErrorMap (Union '[e]) m e m', Typeable e) => m a -> m' a
singleError = emap $ id @> typesExhausted

liftError :: (MonadErrorMap e m (Union s) m', e :∈ s, Typeable e) => m a -> m' a
liftError = emap liftUnion

type Throws e s m = (MonadError (Union s) m, e :∈ s)

data MyError = MyError deriving (Show)
data Another = Another deriving (Show)

-- throwing :: (Throws MyError s m) => m ()
throwing :: (MonadError (Union s) m, '[MyError] :< s) => m ()
throwing = throw MyError

again :: (Throws MyError s m, Throws Another s m) => m ()
again = throwing *> throw Another *> pure ()

discardAnother :: Another -> MyError
discardAnother = const MyError

-- catching :: Either MyError ()
-- catching = runExcept $ singleError $ rethrow (\(x :: Another) -> MyError) again

-- handle m f = catchError m go where
--     go s | Right e <- restrict s = f e
--          | otherwise             = throwError $ reUnion s

