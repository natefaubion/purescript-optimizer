module Language.PureScript.Optimizer.Types where

import Prelude

import Language.PureScript.CoreFn as C

data OptimizerAnn = OptimizerAnn
  { optCfnAnn :: C.Ann
  }

data OptimizerResult = OptimizerResult
  { optModule :: C.Module OptimizerAnn
  }
