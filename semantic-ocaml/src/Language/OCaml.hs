-- | Semantic functionality for OCaml programs.
module Language.OCaml
( Term(..)
, Language.OCaml.Grammar.tree_sitter_ocaml
) where

import           Data.Proxy
import qualified Language.OCaml.AST as OCaml
import qualified Language.OCaml.Tags as OCamlTags
import qualified Tags.Tagging.Precise as Tags
import qualified Language.OCaml.Grammar (tree_sitter_ocaml)
import qualified AST.Unmarshal as TS

newtype Term a = Term { getTerm :: OCaml.SourceFile a }

instance TS.SymbolMatching Term where
  matchedSymbols _ = TS.matchedSymbols (Proxy :: Proxy OCaml.SourceFile)
  showFailure _ = TS.showFailure (Proxy :: Proxy OCaml.SourceFile)

instance TS.Unmarshal Term where
  matchers = fmap (fmap (TS.hoist Term)) TS.matchers

instance Tags.ToTags Term where
  tags src = Tags.runTagging src . OCamlTags.tags . getTerm
