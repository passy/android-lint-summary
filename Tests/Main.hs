{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Test.QuickCheck

import AndroidLintSummary

main :: IO ()
main = hspec $ do
    describe "test" $ do
        it "runs" . property $
            \x -> (read . show $ x) == (x :: Int)

        it "does stuff" $ do
            supportedLintFormatVersion `shouldBe` "4.5"
