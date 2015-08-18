{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Monad.Except.Union ( MonadErrorMap (emap), (:∈), (:⊆), Throws
                                  , (@:), throw, unify, rethrow
                                  , singleError, liftError
                                  , E.Except, ExceptT, throwError
                                  , E.runExceptT, E.runExcept ) where

import Control.Arrow (left)
import Control.Monad.Except (MonadError, ExceptT, withExceptT, throwError)
import qualified Control.Monad.Except as E
import Data.OpenUnion ((:<), (:\), Union, (@>), reUnion, liftUnion, restrict, typesExhausted)
import Data.Typeable (Typeable)
import GHC.Prim (Constraint)

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

-- instance (MonadErrorMap e m e' m', MFunctor t, MonadError e (t m), MonadError e' (t m')) => MonadErrorMap e (t m) e' (t m') where
--     emap f = hoist $ emap f

type family (:∈) (x :: *) (ys :: [*]) :: Constraint where
    x :∈ '[y]      = (x ~ y)
    x :∈ (x ': ys) = ()
    x :∈ (y ': ys) = x :∈ ys

type family (:⊆) (s :: [*]) (s' :: [*]) :: Constraint where
    '[]      :⊆ s' = ()
    (a ': s) :⊆ s' = (s :⊆ s', a :∈ s')

instance (s :⊆ s') => s :< s'

throw :: (MonadError (Union s) m, e :∈ s, Typeable e) => e -> m a
throw = throwError . liftUnion

unify :: ( MonadErrorMap (Union s) m (Union s') m'
         , s ~ (e ': s')
         , (s' :\ e) :⊆ s'
         , Typeable e, Typeable e') =>
         (e -> e') -> m a -> m' a
unify f = emap f' where
    f' s = case restrict s of
        Left  u -> reUnion u
        Right e -> liftUnion $ f e

rethrow :: ( MonadErrorMap (Union s) m (Union s') m'
           , s ~ (e ': (s' :\ e'))
           , e' :∈ s'
           , (s' :\ e' :\ e) :⊆ s'
           , Typeable e, Typeable e' ) =>
           (e -> e') -> m a -> m' a
rethrow f = emap f' where
    f' s = case restrict s of
        Left  u -> reUnion u
        Right e -> liftUnion $ f e

(@:) :: (s ~ (s :\ e), Typeable e) => (e -> e') -> (Union s -> e') -> Union (e ': s) -> e'
r @: l = either l r . restrict
infixr 2 @:

singleError :: (MonadErrorMap (Union '[e]) m e m', Typeable e) => m a -> m' a
singleError = emap $ id @> typesExhausted

liftError :: (MonadErrorMap e m (Union s) m', e :∈ s, Typeable e) => m a -> m' a
liftError = emap liftUnion

type Throws e s m = (MonadError (Union s) m, e :∈ s)
