module Control.Abstract.Primitive where

import Control.Abstract.Addressable
import Control.Abstract.Context
import Control.Abstract.Environment
import Control.Abstract.Evaluator
import Control.Abstract.Heap
import Control.Abstract.Value
import Data.Abstract.Environment
import Data.Abstract.FreeVariables
import Data.ByteString.Char8 (pack, unpack)
import Data.Semigroup.Reducer hiding (unit)
import Data.Semilattice.Lower
import Prologue

builtin :: ( HasCallStack
           , Members '[ Allocator location value
                      , Reader (Environment location value)
                      , Reader ModuleInfo
                      , Reader Span
                      , State (Environment location value)
                      , State (Heap location (Cell location) value)
                      ] effects
           , Ord location
           , Reducer value (Cell location value)
           )
        => String
        -> Evaluator location value effects value
        -> Evaluator location value effects ()
builtin n def = withCurrentCallStack callStack $ do
  let name' = name ("__semantic_" <> pack n)
  addr <- alloc name'
  modifyEnv (insert name' addr)
  def >>= assign addr

defineBuiltins :: ( AbstractValue location value effects
                  , HasCallStack
                  , Members '[ Allocator location value
                             , Reader (Environment location value)
                             , Reader ModuleInfo
                             , Reader Span
                             , Resumable (EnvironmentError value)
                             , State (Environment location value)
                             , State (Heap location (Cell location) value)
                             , Trace
                             ] effects
                  , Ord location
                  , Reducer value (Cell location value)
                  )
               => Evaluator location value effects ()
defineBuiltins =
  builtin "print" (closure ["s"] lowerBound (variable "s" >>= asString >>= trace . unpack >> unit))
