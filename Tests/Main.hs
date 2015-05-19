{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "test" $ do
        it "runs" . property $
          \x -> (read . show $ x) == (x :: Int)
