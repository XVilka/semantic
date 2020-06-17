{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}


module Language.OCaml.AST
( module Language.OCaml.AST
, OCaml.getTestCorpusDir
) where

import           AST.GenerateSyntax
import           AST.Token
import           Language.Haskell.TH.Syntax (runIO)
import qualified TreeSitter.OCaml as OCaml (getNodeTypesPath, getTestCorpusDir, tree_sitter_ocaml)

runIO OCaml.getNodeTypesPath >>= astDeclarationsForLanguage OCaml.tree_sitter_ocaml
