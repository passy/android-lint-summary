{-# LANGUAGE OverloadedStrings #-}

module Main where

import           AndroidLintSummary.CLI     (runCLI)
import           Paths_android_lint_summary (version)

main :: IO ()
main = runCLI version
