{-# LANGUAGE FlexibleContexts #-}

import Test.Hspec

import Control.Monad.Except.Union

data MyError = MyError deriving (Show)
data Another = Another deriving (Show)
data Yet     = Yet     deriving (Show)

raising :: (Raises MyError s m) => m ()
raising = raise MyError

again :: (Raises MyError s m, Raises Another s m) => m ()
again = raising *> raise Another *> pure ()

discardAnother :: Another -> MyError
discardAnother = const MyError

catching :: Either MyError ()
catching = runExcept $ singleError $ raify (\Another -> MyError) again

catching2 :: Either Yet ()
catching2 = runExcept $ singleError $ reraise (\MyError -> Yet) raising

catching3 :: Either Yet ()
catching3 = runExcept $ singleError $ raify (\MyError -> Yet) $ reraise (\Another -> Yet) again

catching4 :: Either MyError ()
catching4 = runExcept $ singleError $ do
    raising
    raify (\Another -> MyError) again

spec :: Spec
spec = describe "a bunch of functions" $
    it "compiles properly" $ True `shouldBe` True

main :: IO ()
main = hspec spec
