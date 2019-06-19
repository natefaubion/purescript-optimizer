module Language.PureScript.Optimizer.Simplify where

import Prelude

import qualified Data.DList as D
import Data.Foldable (foldl')
import Language.PureScript.Optimizer.CoreAnf
import Lens.Micro ((^.))

reassociate :: Expr a -> Expr a
reassociate = run
  where
  run expr =
    stop (expr ^. exprAnn) . go D.empty $ expr

  stop ann (acc, expr) =
    case D.toList acc of
      [] -> expr
      bs -> Let ann bs expr

  go acc = \case
    Let ann bs expr -> do
      let
        bindingFn x = \case
          NonRec a b c -> do
            let
              (x', c') =
                go x c
            D.snoc x' $ NonRec a b c'
          Rec bs' -> do
            D.snoc x
              . Rec
              . fmap (fmap run)
              $ bs'
      go (foldl' bindingFn acc bs) expr
    Abs ann ns expr ->
      (acc, Abs ann ns . run $ expr)
    Case ann ns alts -> do
      let
        altFn (CaseAlternative bs res) = do
          let
            res' = case res of
              Guarded gs ->
                Guarded . fmap (\(a, b) -> (run a, run b)) $ gs
              Unguarded g ->
                Unguarded . run $ g
          CaseAlternative bs res'
      (acc, Case ann ns . fmap altFn $ alts)
    expr ->
      (acc, expr)



