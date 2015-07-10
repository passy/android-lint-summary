{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           BasicPrelude                 hiding (fromString)

import           AndroidLintSummary
import           Options.Applicative
import           Rainbow

import           Control.Monad.Reader         (runReader)
import           Data.Default                 (def)
import           Data.Stringable              (Stringable (fromString, toString))
import           Data.Version                 (showVersion)
import           Paths_android_lint_summary   (version)
import           System.Directory             (getCurrentDirectory)

import qualified System.Console.Terminal.Size as Terminal
import qualified System.FilePath.Find         as Find

appOpts :: Parser AppOpts
appOpts = AppOpts
    <$> optional ( some $ argument str (metavar "FILES") )
    <*> strOption ( long "glob"
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

findFilesFromArgs :: AppOpts -> IO [FilePath]
findFilesFromArgs args' = go $ targets args'
  where
    go (Just names) = return names
    go Nothing = do
      dir <- getCurrentDirectory
      Find.find Find.always (Find.filePath Find.~~? pattern args') dir

main :: IO ()
main = execParser opts' >>= run
  where
    opts' = info (helper <*> appOpts <**> versionInfo)
        ( fullDesc
       <> progDesc "Format Android Lint XML output nicely"
       <> header "android-lint-summary - a lint-results.xml pretty printer" )

    versionInfo = infoOption ("android-lint-summary " ++ showVersion version)
        ( short 'V'
       <> long "version"
       <> hidden
       <> help "Show version information" )

    run :: AppOpts -> IO ()
    run args' = do
        size <- Terminal.size
        let env = AppEnv args' size
        files <- findFilesFromArgs args'
        lintIssues <- concat <$> forM files (openXMLFile >=> readLintIssues)
        mapM_ putChunk $ runReader (formatLintIssues (formatter args') lintIssues) env
