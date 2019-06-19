module Language.PureScript.Optimizer where

import Prelude

import Data.Map (Map)
import qualified Language.PureScript.CoreFn as C
import qualified Language.PureScript.Names as N
import Language.PureScript.Optimizer.CoreAnf
import Language.PureScript.Optimizer.Simplify
import Language.PureScript.Optimizer.Rename
import Language.PureScript.Optimizer.Types

optimize
  :: Map N.ModuleName OptimizerResult
  -> C.Module C.Ann
  -> OptimizerResult
optimize deps cfnMod =
  OptimizerResult $ anfMod { modDecls = modDecls' }
  where
  deps' = optModule <$> deps
  anfMod = renameModule deps' cfnMod
  modDecls' = fmap (fmap declFn) . modDecls $ anfMod

  declFn = \case
    DeclExpr ns expr ->
      DeclExpr ns . reassociate $ expr
    decl ->
      decl


