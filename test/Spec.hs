{-# LANGUAGE FlexibleContexts #-}

import Test.Hspec

import Control.Monad.Except.Union

data MyError = MyError deriving (Show)
data Another = Another deriving (Show)
data Yet     = Yet     deriving (Show)

throwing :: (Throws MyError s m) => m ()
throwing = throw MyError

again :: (Throws MyError s m, Throws Another s m) => m ()
again = throwing *> throw Another *> pure ()

discardAnother :: Another -> MyError
discardAnother = const MyError

catching :: Either MyError ()
catching = runExcept $ singleError $ unify (\Another -> MyError) again

catching2 :: Either Yet ()
catching2 = runExcept $ singleError $ rethrow (\MyError -> Yet) throwing

catching3 :: Either Yet ()
catching3 = runExcept $ singleError $ unify (\MyError -> Yet) $ rethrow (\Another -> Yet) again

catching4 :: Either MyError ()
catching4 = runExcept $ singleError $ do
    throwing
    unify (\Another -> MyError) again

spec :: Spec
spec = describe "a bunch of functions" $
    it "compiles properly" $ True `shouldBe` True

main :: IO ()
main = hspec spec
