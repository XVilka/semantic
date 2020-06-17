{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import           AST.TestHelpers
import           AST.Unmarshal (parseByteString)
import qualified Language.OCaml.AST as OCaml
import qualified System.Path as Path
import           Test.Tasty
import Control.Monad (liftM)

main :: IO ()
main
  =   Path.absDir <$> OCaml.getTestCorpusDir
  >>= excludeMacrosCorpus . readCorpusFiles'
  >>= traverse (testCorpus parse)
  >>= defaultMain . tests
  where
    parse = parseByteString @OCaml.SourceFile @() tree_sitter_ocaml
    excludeMacrosCorpus l = liftM (filter (f "expressions") ) l
      where f p bn = p /= (Path.toString . Path.takeBaseName) bn

tests :: [TestTree] -> TestTree
tests = testGroup "tree-sitter-ocaml corpus tests"
