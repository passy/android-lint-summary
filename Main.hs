{-# LANGUAGE OverloadedStrings, Arrows, NoImplicitPrelude, ExistentialQuantification #-}

module Main where

import BasicPrelude hiding (fromString)

import Rainbow
import Options.Applicative
import AndroidLintSummary

import Control.Monad.Reader (runReader, ask, Reader())
import Data.Default (Default(), def)
import Data.Stringable (Stringable(..))
import Data.Version (showVersion)
import Paths_android_lint_summary (version)
import System.Directory (getCurrentDirectory)
import System.FilePath.GlobPattern (GlobPattern)

import qualified System.FilePath.Find as Find
import qualified Data.Text as T
import qualified System.Console.Terminal.Size as Terminal

appArgs :: Parser AppArgs
appArgs = AppArgs
    <$> strOption ( long "glob"
                 <> short 'g'
                 <> help "Glob pattern to select result files"
                 <> value (pattern def)
                 <> showDefault )
    <*> ( fromString <$>
          strOption ( long "formatter"
                   <> short 'f'
                   <> help "Specify a formatter to use [simple|null]"
                   <> value (toString . formatter $ def)
                   <> showDefault ) )
    <*> flag Normal Verbose ( long "verbose"
                           <> short 'v'
                           <> help "Enable verbose mode" )

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> appArgs <**> versionInfo)
        ( fullDesc
       <> progDesc "Format Android Lint XML output nicely"
       <> header "android-lint-summary - a lint-results.xml pretty printer" )

    versionInfo = infoOption ("android-lint-summary " ++ showVersion version)
      ( short 'V'
     <> long "version"
     <> hidden
     <> help "Show version information" )

    run :: AppArgs -> IO ()
    run args' = do
        dir <- getCurrentDirectory
        size <- Terminal.size
        let env = AppEnv args' size
        files <- Find.find Find.always (Find.filePath Find.~~? pattern args') dir
        lintIssues <- concat <$> forM files (openXMLFile >=> readLintIssues)
        mapM_ putChunk $ runReader (formatLintIssues (formatter args') lintIssues) env
