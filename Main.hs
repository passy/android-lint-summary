{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T

import qualified Rainbow as R
import qualified Rainbow.Types as Y

main :: IO ()
main = do
    R.putChunkLn $ Y.chunkFromLazyText "Some blue text" & fore blue
    putStrLn "Hi there."
