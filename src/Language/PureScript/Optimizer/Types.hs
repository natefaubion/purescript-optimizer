module Language.PureScript.Optimizer.Types where

import Prelude

import qualified Language.PureScript.CoreFn as C
import Language.PureScript.Optimizer.CoreAnf

data OptimizerResult = OptimizerResult
  { optModule :: Module C.Ann
  }
