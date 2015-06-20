{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T

import Rainbow
import System.Directory
import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs

import Data.Foldable (forM_)
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

formatLintResults :: FilePath -> IO ()
formatLintResults filename = do
    contents <- readFile filename
    let doc = readString [withWarnings yes] contents
    xmlIssues <- runX $ doc >>> selectIssues
    print $ readIssue <$> xmlIssues
    where
        readIssue :: XmlTree -> LintIssue
        readIssue (NTree (XTag name trees) _) = LintIssue ErrorSeverity "Hi"

selectIssues :: ArrowXml a => a XmlTree XmlTree
selectIssues = deep $
    isElem >>> hasName "issues"
    >>>
    hasAttrValue "format" (== supportedLintFormatVersion)
    >>>
    (deep $ isElem >>> hasName "issue")

main :: IO ()
main = do
    dir <- getCurrentDirectory
    files <- Find.find Find.always (Find.fileName Find.~~? "lint-results.xml") dir
    forM_ files formatLintResults
    -- forM_ files (\x -> putChunkLn $ chunk x & fore blue)
