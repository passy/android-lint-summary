{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Main where

import BasicPrelude hiding (fromString)

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Monadic

import AndroidLintSummary
import Data.Stringable (Stringable(toString, fromString))


instance Arbitrary LintSeverity where
    arbitrary = elements allSeverities

instance Arbitrary LintFormatter where
    arbitrary = elements allFormatters

allSeverities :: [LintSeverity]
allSeverities = [minBound ..]

allFormatters :: [LintFormatter]
allFormatters = [minBound ..]

purifyException :: (a -> IO b) -> a -> IO (Maybe b)
purifyException f x = protect (const Nothing) $ return . Just =<< f x

main :: IO ()
main = hspec $ do
    describe "LintSeverity" $ do
        it "fromString . toString = id" . property $ do
          \x -> (fromString . toString) x == (x :: LintSeverity)

    describe "LintFormatter" $ do
        it "fromString . toString = id" . property $ do
          \x -> (fromString . toString) x == (x :: LintFormatter)
