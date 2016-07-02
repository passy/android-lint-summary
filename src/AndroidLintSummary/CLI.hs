{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module AndroidLintSummary.CLI where

import           BasicPrelude                 hiding (fromString)

import           AndroidLintSummary
import           Control.Lens                 ((^.))
import           Options.Applicative
import           Rainbow

import           Control.Monad.Reader         (runReader)
import           Data.Default                 (def)
import           Data.Stringable              (Stringable (fromString, toString))
import           Data.Version                 (Version (), showVersion)
import           System.Directory             (getCurrentDirectory)

import qualified System.Console.Terminal.Size as Terminal
import qualified System.FilePath.Find         as Find

findFilesFromArgs :: AppOpts -> IO [FilePath]
findFilesFromArgs args' = go $ args' ^. targets
  where
    go (Just names) = return names
    go Nothing = do
      dir <- getCurrentDirectory
      Find.find Find.always (Find.filePath Find.~~? (args' ^. glob)) dir

lintSummaryParser :: Version -> ParserInfo AppOpts
lintSummaryParser version =
    info (helper <*> appOpts <**> versionInfo)
        ( fullDesc
       <> progDesc "Format Android Lint XML output nicely"
       <> header "android-lint-summary - a lint-results.xml pretty printer" )
  where
    appOpts = AppOpts
        <$> optional ( some $ argument str (metavar "FILES") )
        <*> strOption ( long "glob"
                     <> short 'g'
                     <> help "Glob pattern to select result files"
                     <> value (def ^. glob)
                     <> showDefault )
        <*> ( fromString <$>
              strOption ( long "formatter"
                       <> short 'f'
                       <> help "Specify a formatter to use [simple|null]"
                       <> value (toString $ def ^. formatter)
                       <> showDefault ) )
        <*> flag Normal Verbose ( long "verbose"
                               <> short 'v'
                               <> help "Enable verbose mode" )

    versionInfo = infoOption ("android-lint-summary " ++ showVersion version)
        ( short 'V'
       <> long "version"
       <> hidden
       <> help "Show version information" )

runCLI :: Version -> IO ()
runCLI version = execParser (lintSummaryParser version) >>= run
  where
    run :: AppOpts -> IO ()
    run args' = do
        size <- Terminal.size
        let env = AppEnv args' size
        files <- findFilesFromArgs args'
        lintIssues <- concat <$> forM files (openXMLFile >=> readLintIssues)
        mapM_ putChunk $ runReader (formatLintIssues (args' ^. formatter) lintIssues) env
