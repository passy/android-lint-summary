{-# LANGUAGE OverloadedStrings, Arrows #-}

import qualified Data.Text as T

import Rainbow
import System.Directory
import Text.XML.HXT.Core

import Data.Foldable (forM_)
import Data.Traversable (forM)
import Data.List (sortOn)
import Data.Stringable (Stringable(..))

import qualified System.FilePath.Find as Find


supportedLintFormatVersion :: String
supportedLintFormatVersion = "4"

data LintSeverity = FatalSeverity
                  | ErrorSeverity
                  | WarningSeverity
                  | InformationalSeverity
    deriving (Eq, Ord, Show)

data LintIssue = LintIssue { severity :: LintSeverity
                           , summary :: String
                           , priority :: Int
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
    formatLintIssues :: a -> [LintIssue] -> String

data NullLintFormatter

instance LintFormatter NullLintFormatter where
    formatLintIssues _ _ = ""

readLintIssues :: FilePath -> IO [LintIssue]
readLintIssues filename = do
    contents <- readFile filename
    let doc = readString [withWarnings yes] contents
    runX $ doc >>> selectIssues >>> parseIssues
    where
        parseIssues = proc i -> do
            severity' <- arr fromString <<< getAttrValue "severity" -< i
            summary' <- getAttrValue "summary" -< i
            priority' <- arr read <<< getAttrValue "priority" -< i
            returnA -< LintIssue { severity = severity'
                                 , summary = summary'
                                 , priority = priority'
                                 }

        selectIssues :: ArrowXml a => a XmlTree XmlTree
        selectIssues = getChildren
            >>>
            isElem >>> hasName "issues"
            >>>
            hasAttrValue "format" (== supportedLintFormatVersion)
            >>>
            (deep $ isElem >>> hasName "issue")

printLintIssue :: LintIssue -> IO ()
printLintIssue lrs = do
    printC $  (toString $ severity lrs) <> ": " <> summary lrs
    where
        printC = putChunkLn . colorSeverity (severity lrs) . chunk

main :: IO ()
main = do
    dir <- getCurrentDirectory
    files <- Find.find Find.always (Find.fileName Find.~~? "lint-results.xml") dir
    lintIssues <- forM files readLintIssues
    let sortedLintIssues = sortOn priority (concat lintIssues)
    forM_ sortedLintIssues printLintIssue
