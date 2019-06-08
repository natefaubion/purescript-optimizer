module Language.PureScript.Optimizer where

import Prelude

import Data.Map (Map)
import qualified Language.PureScript.CoreFn as C
import qualified Language.PureScript.Names as N
import Language.PureScript.Optimizer.Types

optimize
  :: Map N.ModuleName OptimizerResult
  -> C.Module C.Ann
  -> OptimizerResult
optimize deps cfnMod@(C.Module {..}) = do
  let
    -- CoreFn Module doesn't have a functor instance for some reason
    cfnMod' = cfnMod
      { C.moduleImports = (\(a, b) -> (OptimizerAnn a, b)) <$> moduleImports
      , C.moduleDecls = fmap OptimizerAnn <$> moduleDecls
      }
  OptimizerResult cfnMod'

