{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
-- | Parsers and pretty printers for the `lint-results.xml` file format.
module AndroidLintSummary (
  supportedLintFormatVersion
, AppEnv(..)
, AppOpts(..)
, LintSeverity(..)
, LintFormatter(..)
, LintLocation()
, LintIssue()
, Verbosity(..)
, readLintIssues
, openXMLFile
, indentWrap
, formatLintIssues
, filename
, line
, column
, severity
, summary
, priority
, explanation
, location
, formatter
, glob
, targets
, verbose
) where

import           BasicPrelude                 hiding (fromString)

import           Rainbow
import           Text.XML.HXT.Core

import           Control.Lens                 hiding (deep)
import           Control.Monad.Reader         (Reader (), ask)
import           Data.Default                 (Default (), def)
import           Data.Stringable              (Stringable (..))
import           System.FilePath.GlobPattern  (GlobPattern)
import           System.IO                    (Handle (), IOMode (ReadMode),
                                               openFile, stdin)

import qualified Data.Text                    as T
import qualified Data.Text.IO                 as TIO
import qualified System.Console.Terminal.Size as Terminal

supportedLintFormatVersion :: String
supportedLintFormatVersion = "4"

defaultLintResultsGlob :: GlobPattern
defaultLintResultsGlob = "**/build/outputs/lint-results.xml"

data LintSeverity = FatalSeverity
                  | ErrorSeverity
                  | WarningSeverity
                  | InformationalSeverity
    deriving (Eq, Ord, Show, Bounded, Enum)

data LintLocation = LintLocation { _filename :: FilePath
                                 , _line     :: Maybe Int
                                 , _column   :: Maybe Int
                                 }
    deriving (Eq, Show)

makeLenses ''LintLocation

data LintIssue = LintIssue { _severity    :: LintSeverity
                           , _summary     :: T.Text
                           , _priority    :: Int
                           , _explanation :: T.Text
                           , _location    :: LintLocation
                           }
    deriving (Eq, Show)

makeLenses ''LintIssue

data LintFormatter =
    NullLintFormatter -- ^ A formatter that doesn't output
                      --   anything.
  | SimpleLintFormatter -- ^ A formatter that displays the errors
                        --   in descending errors with simple color
                        --   coding.
  deriving (Eq, Show, Bounded, Enum)

data Verbosity = Normal | Verbose
    deriving (Show, Eq)

data AppOpts = AppOpts { _targets   :: Maybe [FilePath]
                       , _glob      :: GlobPattern
                       , _formatter :: LintFormatter
                       , _verbose   :: Verbosity
                       }
    deriving (Show, Eq)

makeLenses ''AppOpts

data AppEnv = AppEnv { _opts         :: AppOpts
                     , _terminalSize :: Maybe (Terminal.Window Int)
                     }

makeLenses ''AppEnv

instance Default AppOpts where
    def = AppOpts { _targets = mempty
                  , _glob = defaultLintResultsGlob
                  , _formatter = SimpleLintFormatter
                  , _verbose = Normal
                  }

instance Stringable LintSeverity where
    toString = formatSeverity
    fromString s
        | s == "Fatal" = FatalSeverity
        | s == "Error" = ErrorSeverity
        | s == "Warning" = WarningSeverity
        | s == "Information" = InformationalSeverity
        | otherwise = error $ "Invalid severity " <> s
    length _ = 0


instance Stringable LintFormatter where
    toString NullLintFormatter = "null"
    toString SimpleLintFormatter = "simple"
    fromString s
      | s == "null" = NullLintFormatter
      | s == "simple" = SimpleLintFormatter
      | otherwise = error "Invalid LintFormatter specification"
    length _ = 0

formatSeverity :: LintSeverity -> String
formatSeverity FatalSeverity         = "Fatal"
formatSeverity ErrorSeverity         = "Error"
formatSeverity WarningSeverity       = "Warning"
formatSeverity InformationalSeverity = "Information"

colorSeverity :: LintSeverity -> Chunk a -> Chunk a
colorSeverity FatalSeverity         a = a & fore red & bold
colorSeverity ErrorSeverity         a = a & fore red
colorSeverity WarningSeverity       a = a & fore yellow
colorSeverity InformationalSeverity a = a & fore white


formatLintIssues :: LintFormatter -> [LintIssue] -> Reader AppEnv [Chunk T.Text]
formatLintIssues NullLintFormatter _ = pure mempty
formatLintIssues SimpleLintFormatter issues = concat <$> mapM fmt sortedIssues
    where
        sortedIssues = sortOn (((-1) *) . view priority) issues

        fmt :: LintIssue -> Reader AppEnv [Chunk T.Text]
        fmt i =
          sequence [ pure $ label i
                   , pure $ chunk (" " <> i ^. summary <> "\n") & bold
                   , pure $ chunk $ concat $ replicate 4 " "
                   , pure $ chunk ( T.pack (i ^. location . filename)
                                   <> fmtLine (i ^. location . line)
                                   <> "\n"
                                    ) & underline & fore blue
                   , fmtExplanation i
                   ]

        fmtExplanation :: LintIssue -> Reader AppEnv (Chunk T.Text)
        fmtExplanation i = ask >>= \env -> return $ case env ^. opts . verbose of
          Normal -> mempty
          Verbose -> chunk
            ( maybe
              (i ^. explanation <> "\n")
              (\size -> indentWrap size 4 $ i ^. explanation)
              (env ^. terminalSize)
            ) & faint

        fmtLine :: Show a => Maybe a -> T.Text
        fmtLine = maybe mempty ((":" <>) . T.pack . show)

        label i = dye i ( "["
                       <> T.take 1 (toText $ i ^. severity)
                       <> "]" )

        dye i j = colorSeverity (i ^. severity) (chunk j)

atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

sread :: Read a => String -> a
sread = read . T.pack

sreadMay :: Read a => String -> Maybe a
sreadMay = readMay . T.pack

indentWrap :: Terminal.Window Int -> Int -> T.Text -> T.Text
indentWrap size indentation text = foldMap wrap lines'
  where
    indent :: T.Text
    indent = concat $ replicate indentation " "

    lines' = filter (/= mempty) $ lines text
    wrap t
      | t == mempty = mempty
      | otherwise = let (as, bs) = T.splitAt (Terminal.width size - indentation) t
                    in indent <> as <> "\n" <> wrap bs

openXMLFile :: forall s b. FilePath -> IO (IOStateArrow s b XmlTree)
openXMLFile = (readXMLFileHandle =<<) . getHandle
  where
    getHandle filepath
      | filepath == "-" = return stdin
      | otherwise       = openFile filepath ReadMode


readXMLFileHandle :: forall s b. Handle -> IO (IOStateArrow s b XmlTree)
readXMLFileHandle h = do
    contents <- TIO.hGetContents h
    return $ readString [withWarnings yes] $ T.unpack contents

readLintIssues :: IOSLA (XIOState ()) XmlTree XmlTree -> IO [LintIssue]
readLintIssues doc =
    runX $ doc >>> selectIssues >>> parseIssues
    where
        parseIssues :: ArrowXml a => a XmlTree LintIssue
        parseIssues = proc i -> do
            severity' <- arr fromString <<< getAttrValue "severity" -< i
            summary' <- arr T.pack <<< getAttrValue "summary" -< i
            priority' <- arr sread <<< getAttrValue "priority" -< i
            explanation' <- arr T.pack <<< getAttrValue "explanation" -< i
            location' <- parseLocation -< i
            returnA -< LintIssue { _severity = severity'
                                 , _summary = summary'
                                 , _explanation = explanation'
                                 , _priority = priority'
                                 , _location = location'
                                 }

        parseLocation :: ArrowXml a => a XmlTree LintLocation
        parseLocation = atTag "location" >>> proc l -> do
            filename' <- getAttrValue "file" -< l
            line' <- arr sreadMay <<< getAttrValue "line" -< l
            column' <- arr sreadMay <<< getAttrValue "column" -< l
            returnA -< LintLocation { _filename = filename'
                                    , _line = line'
                                    , _column = column'
                                    }

        selectIssues :: ArrowXml a => a XmlTree XmlTree
        selectIssues = getChildren
            >>>
            isElem >>> hasName "issues"
            >>>
            hasAttrValue "format" (== supportedLintFormatVersion)
            >>>
            atTag "issue"
