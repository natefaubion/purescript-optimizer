module Language.PureScript.Optimizer.Simplify where

import Prelude

import Data.Foldable (foldl')
import Language.PureScript.Optimizer.CoreAnf

simplify :: Expr a -> Expr a
simplify = run
  where
  run =
    stop . go []

  stop (acc, expr) =
    foldl' (flip (\(ann, n, a) -> Let ann n a)) expr acc

  go acc = \case
    Let ann n a b -> do
      let
        (acc', a') =
          go acc a
      go ((ann, n, a') : acc') b
    LetRec bs expr ->
      (acc, LetRec (fmap (fmap run) bs) . run $ expr)
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



