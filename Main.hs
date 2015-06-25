{-# LANGUAGE OverloadedStrings, Arrows, NoImplicitPrelude #-}

import BasicPrelude hiding (fromString)

import Rainbow
import Text.XML.HXT.Core
import Options.Applicative

import Control.Monad.Reader (runReader, asks, Reader())
import Data.Stringable (Stringable(..))
import Data.Default (Default(), def)
import System.Directory (getCurrentDirectory)
import System.FilePath.GlobPattern (GlobPattern)

import qualified System.FilePath.Find as Find
import qualified Data.Text as T
import qualified System.Console.Terminal.Size as Terminal

supportedLintFormatVersion :: String
supportedLintFormatVersion = "4"

defaultLintResultsGlob :: GlobPattern
defaultLintResultsGlob = "**/build/outputs/lint-results.xml"

data LintSeverity = FatalSeverity
                  | ErrorSeverity
                  | WarningSeverity
                  | InformationalSeverity
    deriving (Eq, Ord, Show)

data LintLocation = LintLocation { filename :: FilePath
                                 , line :: Maybe Int
                                 , column :: Maybe Int
                                 }
    deriving (Eq, Show)

data LintIssue = LintIssue { severity :: LintSeverity
                           , summary :: T.Text
                           , priority :: Int
                           , explanation :: T.Text
                           , location :: LintLocation
                           }
    deriving (Eq, Show)

formatSeverity :: LintSeverity -> String
formatSeverity FatalSeverity         = "Fatal"
formatSeverity ErrorSeverity         = "Error"
formatSeverity WarningSeverity       = "Warning"
formatSeverity InformationalSeverity = "Informational"

colorSeverity :: LintSeverity -> Chunk a -> Chunk a
colorSeverity FatalSeverity         a = a & fore red & bold
colorSeverity ErrorSeverity         a = a & fore red
colorSeverity WarningSeverity       a = a & fore yellow
colorSeverity InformationalSeverity a = a & fore white

instance Stringable LintSeverity where
    toString = formatSeverity
    fromString s
        | s == "Fatal" = FatalSeverity
        | s == "Error" = ErrorSeverity
        | s == "Warning" = WarningSeverity
        | s == "Informational" = InformationalSeverity
        | otherwise = error "Invalid severity"
    length _ = 0

data LintFormatter =
    NullLintFormatter -- ^ A formatter that doesn't output
                      --   anything.
  | SimpleLintFormatter -- ^ A formatter that displays the errors
                        --   in descending errors with simple color
                        --   coding.
  deriving (Show)

instance Stringable LintFormatter where
    toString NullLintFormatter = "null"
    toString SimpleLintFormatter = "simple"
    fromString s
      | s == "null" = NullLintFormatter
      | s == "simple" = SimpleLintFormatter
      | otherwise = error "Invalid LintFormatter specification"
    length _ = 0

formatLintIssues :: LintFormatter -> [LintIssue] -> Reader AppEnv [Chunk T.Text]
formatLintIssues NullLintFormatter _ = pure mempty
formatLintIssues SimpleLintFormatter issues = concat <$> mapM fmt sortedIssues
    where
        sortedIssues = sortOn priority issues

        fmt :: LintIssue -> Reader AppEnv [Chunk T.Text]
        fmt i =
          sequence [ pure $ label i
                   , pure $ chunk (" " <> summary i <> "\n") & bold
                   , pure $ chunk ( "\t"
                                   <> T.pack (filename $ location i)
                                   <> fmtLine (line $ location i)
                                   <> "\n"
                                    ) & underline & fore blue
                   , fmtExplanation i
                   ]

        fmtExplanation :: LintIssue -> Reader AppEnv (Chunk T.Text)
        fmtExplanation i = asks (verbose . args) >>= \v -> return $ case v of
          Normal -> mempty
          Verbose -> chunk ( "\t"
                           <> explanation i
                           <> "\n"
                           ) & faint

        fmtLine = maybe mempty ((":" <>) . show)

        label i = dye i ( "["
                       <> T.take 1 (toText $ severity i)
                       <> "]" )

        dye = (. chunk) . colorSeverity . severity

atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

sread :: Read a => String -> a
sread = read . T.pack

sreadMay :: Read a => String -> Maybe a
sreadMay = readMay . T.pack

readLintIssues :: FilePath -> IO [LintIssue]
readLintIssues filepath = do
    contents <- readFile filepath
    let doc = readString [withWarnings yes] $ T.unpack contents
    runX $ doc >>> selectIssues >>> parseIssues
    where
        parseIssues :: ArrowXml a => a XmlTree LintIssue
        parseIssues = proc i -> do
            severity' <- arr fromString <<< getAttrValue "severity" -< i
            summary' <- arr T.pack <<< getAttrValue "summary" -< i
            priority' <- arr sread <<< getAttrValue "priority" -< i
            explanation' <- arr T.pack <<< getAttrValue "explanation" -< i
            location' <- parseLocation -< i
            returnA -< LintIssue { severity = severity'
                                 , summary = summary'
                                 , explanation = explanation'
                                 , priority = priority'
                                 , location = location'
                                 }

        parseLocation :: ArrowXml a => a XmlTree LintLocation
        parseLocation = atTag "location" >>> proc l -> do
            filename' <- getAttrValue "file" -< l
            line' <- arr sreadMay <<< getAttrValue "line" -< l
            column' <- arr sreadMay <<< getAttrValue "column" -< l
            returnA -< LintLocation { filename = filename'
                                    , line = line'
                                    , column = column'
                                    }

        selectIssues :: ArrowXml a => a XmlTree XmlTree
        selectIssues = getChildren
            >>>
            isElem >>> hasName "issues"
            >>>
            hasAttrValue "format" (== supportedLintFormatVersion)
            >>>
            atTag "issue"

data Verbosity = Normal | Verbose
    deriving (Show, Eq)

data AppArgs = AppArgs { pattern :: GlobPattern
                       , formatter :: LintFormatter
                       , verbose :: Verbosity
                       }
    deriving (Show)

data AppEnv = AppEnv { args :: AppArgs
                     , terminalSize :: Maybe (Terminal.Window Integer)
                     }

instance Default AppArgs where
    def = AppArgs { pattern = defaultLintResultsGlob
                  , formatter = SimpleLintFormatter
                  , verbose = Normal
                  }

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
    opts = info (helper <*> appArgs)
        ( fullDesc
       <> progDesc "Format Android Lint XML output nicely"
       <> header "android-lint-summary - a lint-results.xml pretty printer" )

    run :: AppArgs -> IO ()
    run args' = do
        dir <- getCurrentDirectory
        size <- Terminal.size
        let env = AppEnv args' size
        files <- Find.find Find.always (Find.filePath Find.~~? pattern args') dir
        lintIssues <- concat <$> forM files readLintIssues
        mapM_ putChunk $ runReader (formatLintIssues (formatter args') lintIssues) env
