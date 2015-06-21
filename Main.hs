{-# LANGUAGE OverloadedStrings, Arrows, ExistentialQuantification #-}

import Rainbow
import Text.XML.HXT.Core

import Data.List (sortOn)
import Data.Stringable (Stringable(..))
import Data.Traversable (forM)
import System.Directory (getCurrentDirectory)

import qualified System.FilePath.Find as Find
import qualified Data.Text as T

supportedLintFormatVersion :: String
supportedLintFormatVersion = "4"

data LintSeverity = FatalSeverity
                  | ErrorSeverity
                  | WarningSeverity
                  | InformationalSeverity
    deriving (Eq, Ord, Show)

data LintLocation = LintLocation { filename :: FilePath
                                 , line :: Int
                                 , column :: Int
                                 }
    deriving (Eq, Show)

data LintIssue = LintIssue { severity :: LintSeverity
                           , summary :: T.Text
                           , priority :: Int
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

class LintFormatter a where
    formatLintIssues :: a -> [LintIssue] -> [Chunk T.Text]

-- | A formatter that doesn't output anything.
data NullLintFormatter = NullLintFormatter

-- | A formatter that displays the errors in descending errors
--   with simple color coding.
data SimpleLintFormatter = SimpleLintFormatter

instance LintFormatter NullLintFormatter where
    formatLintIssues _ _ = pure mempty

instance LintFormatter SimpleLintFormatter where
    formatLintIssues _ issues = concat $ fmt <$> sortedIssues
        where
            sortedIssues = sortOn priority issues
            fmt i = [ label i
                    , chunk $ " " <> summary i <> "\n"
                    , chunk ( "\t"
                            <> T.pack (filename $ location i)
                            <> ":"
                            <> tshow (line $ location i)
                            <> "\n"
                            ) & faint
                    ]
            label i = dye i ( "["
                            <> T.take 1 (toText $ severity i)
                            <> "]" )
            dye = (. chunk) . colorSeverity . severity

tshow :: (Show s) => s -> T.Text
tshow = T.pack . show

atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

readLintIssues :: FilePath -> IO [LintIssue]
readLintIssues filename = do
    contents <- readFile filename
    let doc = readString [withWarnings yes] contents
    runX $ doc >>> selectIssues >>> parseIssues
    where
        parseIssues :: ArrowXml a => a XmlTree LintIssue
        parseIssues = proc i -> do
            severity' <- arr fromString <<< getAttrValue "severity" -< i
            summary' <- arr T.pack <<< getAttrValue "summary" -< i
            priority' <- arr read <<< getAttrValue "priority" -< i
            location' <- parseLocation -< i
            returnA -< LintIssue { severity = severity'
                                 , summary = summary'
                                 , priority = priority'
                                 , location = location'
                                 }

        parseLocation :: ArrowXml a => a XmlTree LintLocation
        parseLocation = atTag "location" >>> proc l -> do
            filename' <- getAttrValue "file" -< l
            line' <- arr read <<< getAttrValue "line" -< l
            column' <- arr read <<< getAttrValue "column" -< l
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

main :: IO ()
main = do
    dir <- getCurrentDirectory
    files <- Find.find Find.always (Find.fileName Find.~~? "lint-results.xml") dir
    lintIssues <- concat <$> forM files readLintIssues
    -- To be based on CLI arguments later
    let formatter = SimpleLintFormatter
    mapM_ putChunk (formatLintIssues formatter lintIssues)
