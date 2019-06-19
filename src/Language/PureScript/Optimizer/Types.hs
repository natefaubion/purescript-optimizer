module Language.PureScript.Optimizer.Types where

import Prelude

import qualified Language.PureScript.AST.SourcePos as Pos
import qualified Language.PureScript.CoreFn as C
import qualified Language.PureScript.Types as T
import Language.PureScript.Optimizer.CoreAnf

data OptimizerAnn = OptimizerAnn
  { annSpan :: Pos.SourceSpan
  , annType :: Maybe T.SourceType
  , annMeta :: Maybe C.Meta
  , annArity :: Int
  }

optAnn :: C.Ann -> OptimizerAnn
optAnn (ss, _, ty, meta) = OptimizerAnn ss ty meta 0

data OptimizerResult = OptimizerResult
  { optModule :: Module OptimizerAnn
  }
