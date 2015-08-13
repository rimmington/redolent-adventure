{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Control.Monad.Except (MonadError, throwError, catchError)
import Data.OpenUnion
-- import Data.Type.List
import Data.Typeable (Typeable, Proxy (Proxy))
import Data.Void (Void)
import GHC.Prim (Constraint, coerce)

-- class Embed a b where
--     embed :: a -> b

-- instance Embed a (Either a b) where
--     embed = Left

-- instance Embed b (Either a b) where
--     embed = Right

-- class (MonadError es m, Find e es ~ True) => Throws e m where
--     embed :: e -> es

-- class ThrowsAll (s :: [*]) m where
--     throw :: ('[e] :< s, Typeable e) => e -> m a

-- type family AsEither (es :: [*]) where
--     AsEither '[x]      = x
--     AsEither (x ': xs) = Either x (AsEither xs)

-- type family Contains e (es :: [*]) :: Constraint where
--     Contains e '[e']         = (e ~ e')
--     Contains e (e ': y ': z) = ()
--     Contains e (x ': y ': z) = Contains e (y ': z)

-- instance (MonadError (Union s) m) => ThrowsAll s m where
--     throw = throwError . liftUnion

throwU :: (MonadError (Union s) m, '[e] :< s, Throws e, Typeable e) => e -> m a
throwU = throwError . liftUnion

handle :: forall a e m. (MonadError e m) => (Throws e => m a) -> (e -> m a) -> m a
handle = catchError . unthrow (Proxy :: Proxy e)

handleU :: forall a e m s. (MonadError (Union s) m, '[e] :< s, Typeable e) => (Throws e => m a) -> (e -> m a) -> m a
handleU m f = unthrow (Proxy :: Proxy e) $ catchError m go where
    go s | Right e <- restrict s = f e
         | otherwise             = throwError s

class Throws e where
type role Throws representational

unthrow :: proxy e -> (Throws e => a) -> a
unthrow _ = unWrap . coerceWrap . Wrap

newtype Wrap e a = Wrap { unWrap :: Throws e => a }

coerceWrap :: Wrap e a -> Wrap (Catch e) a
coerceWrap = coerce

newtype Catch a = Catch a
instance Throws (Catch e) where

-- type Throws e m = forall s. (MonadError (Union s) m, '[e] :< s)

-- instance (MonadError (Union s) m, ) => ThrowsAll (x ': y ': z) m where
--     throw = undefined

-- handle :: forall es e a. (Find e es ~ True) => (e -> a) -> (ThrowsAll es m => m a) -> (ThrowsAll (Remove e es) m => m a)
-- handle = undefined

-- type Throws e m = (Find e es ~ True, ThrowsAll es m)

-- instance (Throws e' m, Embed e e') => Throws e m where
--     throw = throw . embed
