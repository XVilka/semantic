{-# LANGUAGE ApplicativeDo, TemplateHaskell #-}
module Semantic.CLI
( main
-- Testing
, runDiff
, runParse
) where

import           Data.File
import           Data.Language
import           Data.List (intercalate)
import           Data.List.Split (splitWhen)
import           Data.Version (showVersion)
import           Development.GitRev
import           Options.Applicative
import qualified Paths_semantic as Library (version)
import           Prologue
import           Rendering.Renderer
import qualified Semantic.Diff as Semantic (diffBlobPairs)
import           Semantic.Graph as Semantic (graph, GraphType(..))
import           Semantic.IO (languageForFilePath)
import qualified Semantic.Log as Log
import qualified Semantic.Parse as Semantic (parseBlobs, astParseBlobs)
import qualified Semantic.Task as Task
import           System.IO (Handle, stdin, stdout)
import           Text.Read

main :: IO ()
main = customExecParser (prefs showHelpOnEmpty) arguments >>= uncurry Task.runTaskWithOptions

runDiff :: SomeRenderer DiffRenderer -> Either Handle [Both File] -> Task.TaskEff ByteString
runDiff (SomeRenderer diffRenderer) = Semantic.diffBlobPairs diffRenderer <=< Task.readBlobPairs

runParse :: SomeRenderer TermRenderer -> Either Handle [File] -> Task.TaskEff ByteString
runParse (SomeRenderer parseTreeRenderer) = Semantic.parseBlobs parseTreeRenderer <=< Task.readBlobs

runASTParse :: SomeRenderer TermRenderer -> Either Handle [File] -> Task.TaskEff ByteString
runASTParse (SomeRenderer parseTreeRenderer) = Semantic.astParseBlobs parseTreeRenderer <=< Task.readBlobs

runGraph :: Semantic.GraphType -> SomeRenderer GraphRenderer -> Maybe FilePath -> FilePath -> Language -> [FilePath] -> Task.TaskEff ByteString
runGraph graphType (SomeRenderer r) rootDir dir excludeDirs = Semantic.graph graphType r <=< Task.readProject rootDir dir excludeDirs

-- | A parser for the application's command-line arguments.
--
--   Returns a 'Task' to read the input, run the requested operation, and write the output to the specified output path or stdout.
arguments :: ParserInfo (Log.Options, Task.TaskEff ())
arguments = info (version <*> helper <*> ((,) <$> optionsParser <*> argumentsParser)) description
  where
    version = infoOption versionString (long "version" <> short 'v' <> help "Output the version of the program")
    versionString = "semantic version " <> showVersion Library.version <> " (" <> $(gitHash) <> ")"
    description = fullDesc <> header "semantic -- Parse and diff semantically"

    optionsParser = do
      disableColour <- not <$> switch (long "disable-colour" <> long "disable-color" <> help "Disable ANSI colors in log messages even if the terminal is a TTY.")
      logLevel <- options [ ("error", Just Log.Error) , ("warning", Just Log.Warning) , ("info", Just Log.Info) , ("debug", Just Log.Debug) , ("none", Nothing)]
                          (long "log-level" <> value (Just Log.Warning) <> help "Log messages at or above this level, or disable logging entirely.")
      requestId <- optional (strOption $ long "request-id" <> help "A string to use as the request identifier for any logged messages." <> metavar "id")
      failOnWarning <- switch (long "fail-on-warning" <> help "Fail on assignment warnings.")
      pure $ Log.Options disableColour logLevel requestId False False Log.logfmtFormatter 0 failOnWarning

    argumentsParser = do
      subparser <- hsubparser (diffCommand <> parseCommand <>  tsParseCommand <> graphCommand)
      output <- Right <$> strOption (long "output" <> short 'o' <> help "Output path, defaults to stdout") <|> pure (Left stdout)
      pure $ subparser >>= Task.writeToOutput output

    diffCommand = command "diff" (info diffArgumentsParser (progDesc "Show changes between commits or paths"))
    diffArgumentsParser = do
      renderer <- flag  (SomeRenderer SExpressionDiffRenderer) (SomeRenderer SExpressionDiffRenderer) (long "sexpression" <> help "Output s-expression diff tree")
              <|> flag'                                        (SomeRenderer JSONDiffRenderer)        (long "json" <> help "Output JSON diff trees")
              <|> flag'                                        (SomeRenderer ToCDiffRenderer)         (long "toc" <> help "Output JSON table of contents diff summary")
              <|> flag'                                        (SomeRenderer DOTDiffRenderer)         (long "dot" <> help "Output the diff as a DOT graph")
      filesOrStdin <- Right <$> some (both <$> argument filePathReader (metavar "FILE_A") <*> argument filePathReader (metavar "FILE_B")) <|> pure (Left stdin)
      pure $ runDiff renderer filesOrStdin

    parseCommand = command "parse" (info parseArgumentsParser (progDesc "Print parse trees for path(s)"))
    parseArgumentsParser = do
      renderer <- flag  (SomeRenderer SExpressionTermRenderer) (SomeRenderer SExpressionTermRenderer) (long "sexpression" <> help "Output s-expression parse trees (default)")
              <|> flag'                                        (SomeRenderer JSONTermRenderer)        (long "json" <> help "Output JSON parse trees")
              <|> flag'                                        (SomeRenderer TagsTermRenderer)        (long "tags" <> help "Output JSON tags")
              <|> flag'                                        (SomeRenderer . SymbolsTermRenderer)   (long "symbols" <> help "Output JSON symbol list")
                   <*> (option symbolFieldsReader (  long "fields"
                                                 <> help "Comma delimited list of specific fields to return (symbols output only)."
                                                 <> metavar "FIELDS")
                  <|> pure defaultSymbolFields)
              <|> flag'                                        (SomeRenderer ImportsTermRenderer)     (long "import-graph" <> help "Output JSON import graph")
              <|> flag'                                        (SomeRenderer DOTTermRenderer)         (long "dot" <> help "Output DOT graph parse trees")
      filesOrStdin <- Right <$> some (argument filePathReader (metavar "FILES...")) <|> pure (Left stdin)
      pure $ runParse renderer filesOrStdin

    tsParseCommand = command "ts-parse" (info tsParseArgumentsParser (progDesc "Print specialized tree-sitter ASTs for path(s)"))
    tsParseArgumentsParser = do
      renderer <- flag  (SomeRenderer SExpressionTermRenderer) (SomeRenderer SExpressionTermRenderer) (long "sexpression" <> help "Output s-expression ASTs (default)")
              <|> flag'                                        (SomeRenderer JSONTermRenderer)        (long "json" <> help "Output JSON ASTs")
      filesOrStdin <- Right <$> some (argument filePathReader (metavar "FILES...")) <|> pure (Left stdin)
      pure $ runASTParse renderer filesOrStdin

    graphCommand = command "graph" (info graphArgumentsParser (progDesc "Compute a graph for a directory or entry point"))
    graphArgumentsParser = do
      graphType <- flag ImportGraph ImportGraph (long "imports" <> help "Compute an import graph")
               <|> flag'            CallGraph       (long "calls" <> help "Compute a call graph")
      renderer <- flag (SomeRenderer DOTGraphRenderer) (SomeRenderer DOTGraphRenderer)  (long "dot" <> help "Output in DOT graph format (default)")
              <|> flag'                                      (SomeRenderer JSONGraphRenderer) (long "json" <> help "Output JSON graph")
      rootDir <- rootDirectoryOption
      excludeDirs <- excludeDirsOption
      File{..} <- argument filePathReader (metavar "DIRECTORY:LANGUAGE")
      pure $ runGraph graphType renderer rootDir filePath (fromJust fileLanguage) excludeDirs

    rootDirectoryOption = optional (strOption (long "root" <> help "Root directory of project. Optional, defaults to entry file/directory." <> metavar "DIRECTORY"))
    excludeDirsOption = many (strOption (long "exclude-dir" <> help "Exclude a directory (e.g. vendor)"))
    filePathReader = eitherReader parseFilePath
    parseFilePath arg = case splitWhen (== ':') arg of
        [a, b] | lang <- readMaybe b -> Right (File a lang)
               | lang <- readMaybe a -> Right (File b lang)
        [path] -> maybe (Left $ "Cannot identify language for path: " <> path) (Right . File path . Just) (languageForFilePath path)
        args -> Left ("cannot parse `" <> join args <> "`\nexpecting FILE:LANGUAGE or just FILE")

    optionsReader options = eitherReader $ \ str -> maybe (Left ("expected one of: " <> intercalate ", " (fmap fst options))) (Right . snd) (find ((== str) . fst) options)
    options options fields = option (optionsReader options) (fields <> showDefaultWith (findOption options) <> metavar (intercalate "|" (fmap fst options)))
    findOption options value = maybe "" fst (find ((== value) . snd) options)

    -- Example: semantic parse --symbols --fields=symbol,path,language,kind,line,span
    symbolFieldsReader = eitherReader parseSymbolFields
    parseSymbolFields arg = let fields = splitWhen (== ',') arg in
                      Right SymbolFields
                        { symbolFieldsName = "symbol" `elem` fields
                        , symbolFieldsPath = "path" `elem` fields
                        , symbolFieldsLang = "language" `elem` fields
                        , symbolFieldsKind = "kind" `elem` fields
                        , symbolFieldsLine = "line" `elem` fields
                        , symbolFieldsSpan = "span" `elem` fields
                        }
