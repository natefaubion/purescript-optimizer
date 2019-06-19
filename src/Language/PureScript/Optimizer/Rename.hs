module Language.PureScript.Optimizer.Rename where

import Prelude

import Control.Monad (join)
import qualified Control.Monad.State.Class as MS
import qualified Control.Monad.Trans.State.Strict as S
import Data.Coerce (coerce)
import Data.Foldable (fold, toList)
import Lens.Micro ((^.), _3)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Language.PureScript.AST.SourcePos as Pos
import qualified Language.PureScript.CoreFn as C
import qualified Language.PureScript.Names as N
import Language.PureScript.Optimizer.CoreAnf
import Language.PureScript.Optimizer.Types

type Scope = M.Map (N.Qualified N.Ident) Name

data RenameState = RenameState
  { rsModuleName :: N.ModuleName
  , rsFresh :: Int
  , rsScope :: Scope
  }

freshName :: MS.MonadState RenameState m => m Name
freshName = MS.state $ \st@(RenameState {..}) ->
  (Name (coerce rsModuleName) rsFresh, st { rsFresh = rsFresh + 1 })

bindName :: MS.MonadState RenameState m => N.Qualified N.Ident -> m Name
bindName ident = MS.state $ \st@(RenameState {..}) -> do
  let
    name = Name (getQual (coerce rsModuleName) ident) rsFresh
    scope =
      case N.getQual ident of
        Nothing ->
          M.insert ident name
           . M.insert (N.Qualified (Just rsModuleName) . N.disqualify $ ident) name
           $ rsScope
        Just _ ->
          M.insert ident name rsScope
  (name, st { rsFresh = rsFresh + 1, rsScope = scope })

bindName' :: MS.MonadState RenameState m => N.Ident -> m Name
bindName' = bindName . N.Qualified Nothing

assocName :: MS.MonadState RenameState m => N.Qualified N.Ident -> Name -> m ()
assocName ident name = MS.state $ \st@(RenameState {..}) -> do
  let
    scope =
      case N.getQual ident of
        Nothing ->
          M.insert ident name
           . M.insert (N.Qualified (Just rsModuleName) . N.disqualify $ ident) name
           $ rsScope
        Just _ ->
          M.insert ident name rsScope
  ((), st { rsScope = scope })

assocName' :: MS.MonadState RenameState m => N.Ident -> Name -> m ()
assocName' = assocName . N.Qualified Nothing

getName :: MS.MonadState RenameState m => N.Qualified N.Ident -> m Name
getName ident = do
  RenameState { rsModuleName, rsScope } <- MS.get
  pure $ case M.lookup ident rsScope of
    Nothing ->
      error
        (unlines
          $ "getName: unknown free variable " <> show ident
          : show rsModuleName
          : fmap show (M.toList rsScope))
        -- (fromMaybe rsModuleName . N.getQual $ ident, -1)
    Just n -> n

currentModule :: MS.MonadState RenameState m => m N.ModuleName
currentModule = do
  RenameState { rsModuleName } <- MS.get
  pure rsModuleName

scoped :: MS.MonadState RenameState m => m a -> m a
scoped m = do
  RenameState { rsScope } <- MS.get
  res <- m
  MS.modify (\st -> st { rsScope = rsScope })
  pure $ res

renameModule :: M.Map N.ModuleName (Module OptimizerAnn) -> C.Module C.Ann -> Module OptimizerAnn
renameModule env (C.Module {..}) = flip S.evalState state $ do
  frn <- traverse goForeign moduleForeign
  decls <- join <$> traverse goBind moduleDecls
  pure $ Module
    { modName = moduleName
    , modDeps = snd <$> moduleImports
    , modDecls = M.fromList frn <> M.fromList decls
    , modSort = fmap fst frn <> fmap fst decls
    }
  where
  state =
    RenameState moduleName 0 (scope <> M.singleton identUndefined (Name (coerce prim) 0))

  scope =
    fold
      . catMaybes
      . fmap (fmap scopeFn . flip M.lookup env . snd)
      $ moduleImports

  nameFn mn (k, v) =
    (N.Qualified (Just mn) k, fst v)

  scopeFn (Module {..}) =
    M.fromList
      . fmap (nameFn modName)
      . M.toList
      $ modDecls

  goForeign ident = do
    n <- bindName' ident
    pure (ident, (n, DeclForeign ident))

  goBind = \case
    C.NonRec _ ident decl -> do
      n <- bindName' ident
      pure . (ident,) . (n,) <$> goDecl [] decl
    C.Rec bs -> do
      bs' <- traverse (\((_, ident), expr) -> (ident,) . (,expr) <$> bindName' ident) bs
      traverse (traverse (traverse (goDecl (fst . snd <$> bs')))) bs'

  goDecl group = \case
    C.Constructor _ _ _ fields ->
      pure . DeclCtor . length $ fields
    expr -> scoped $ do
      DeclExpr group <$> renameExpr expr

renameExpr :: MS.MonadState RenameState m => C.Expr C.Ann -> m (Expr OptimizerAnn)
renameExpr = goExpr
  where
  goExpr = scoped . \case
    C.Literal ann a -> do
      goLit (optAnn ann) a
    C.Accessor ann p expr -> scoped $ do
      expr' <- goLetExpr expr
      pure $ buildLet [expr'] $ Access (optAnn ann) (letName expr') p
    C.ObjectUpdate ann expr pairs -> do
      expr' <- goLetExpr expr
      pairs' <- traverse (traverse goLetExpr) pairs
      pure
       . buildLet (expr' : fmap snd pairs')
       . Update (optAnn ann) (letName expr')
       . fmap (fmap letName)
       $ pairs'
    C.Abs ann n expr -> do
      n' <- bindName' n
      Abs (optAnn ann) [n'] <$> goExpr expr
    C.App ann a b -> do
      a' <- goLetExpr a
      b' <- goLetExpr b
      pure
       . buildLet [a', b']
       $ App (optAnn ann) (letName a') [letName b']
    C.Let ann bs expr -> do
      bs' <- catMaybes <$> traverse goBind bs
      expr' <- goExpr expr
      pure $ Let (optAnn ann) bs' expr'
    C.Case ann exprs alts -> do
      exprs' <- traverse goLetExpr exprs
      alts' <- traverse goAlt alts
      pure
        . buildLet exprs'
        . flip (Case (optAnn ann)) alts'
        . fmap letName
        $ exprs'
    C.Var ann v ->
      Var (optAnn ann) <$> getName v
    C.Constructor _ _ ident _ ->
      error $ "renameExpr: Invalid constructor " <> show ident

  goLit ann = \case
    C.ArrayLiteral exprs -> scoped $ do
      exprs' <- traverse goLetExpr exprs
      pure
        . buildLet exprs'
        . Lit ann
        . C.ArrayLiteral
        . fmap letName
        $ exprs'
    C.ObjectLiteral pairs -> scoped $ do
      pairs' <- traverse (traverse goLetExpr) pairs
      pure
        . buildLet (fmap snd pairs')
        . Lit ann
        . C.ObjectLiteral
        . fmap (fmap letName)
        $ pairs'
    C.NumericLiteral x ->
      pure . Lit ann . C.NumericLiteral $ x
    C.StringLiteral x ->
      pure . Lit ann . C.StringLiteral $ x
    C.CharLiteral x ->
      pure . Lit ann . C.CharLiteral $ x
    C.BooleanLiteral x ->
      pure . Lit ann . C.BooleanLiteral $ x

  goBind = \case
    C.NonRec _ a (C.Var _ b) -> do
      n <- getName b
      assocName' a n
      pure $ Nothing
    C.NonRec ann ident expr -> do
      n <- bindName' ident
      expr' <- goExpr expr
      pure . Just $ NonRec (optAnn ann) n expr'
    C.Rec bs -> do
      let
        bindFn ((ann, ident), expr) =
          (,expr) . (optAnn ann,) <$> bindName' ident
      bs' <- traverse bindFn bs
      bs'' <- traverse (traverse goExpr) bs'
      pure . Just $ Rec bs''

  goAlt (C.CaseAlternative bs res) =
    scoped $ CaseAlternative
      <$> traverse goBinder bs
      <*> goResult res

  goBinder = \case
    C.NullBinder ann ->
      pure $ BinderWildcard (optAnn ann)
    C.LiteralBinder ann lit ->
      BinderLit (optAnn ann) <$> goBinderLit lit
    C.VarBinder ann ident ->
      BinderVar (optAnn ann) <$> bindName' ident
    C.NamedBinder ann ident b ->
      BinderNamed (optAnn ann) <$> bindName' ident <*> goBinder b
    C.ConstructorBinder ann _ ctor args ->
      BinderCtor (optAnn ann) <$> getName (ctorToIdent <$> ctor) <*> traverse goBinder args

  goBinderLit = \case
    C.ArrayLiteral bs ->
      C.ArrayLiteral <$> traverse goBinder bs
    C.ObjectLiteral pairs ->
      C.ObjectLiteral <$> traverse (traverse goBinder) pairs
    C.NumericLiteral x ->
      pure $ C.NumericLiteral x
    C.StringLiteral x ->
      pure $ C.StringLiteral x
    C.CharLiteral x ->
      pure $ C.CharLiteral x
    C.BooleanLiteral x ->
      pure $ C.BooleanLiteral x

  goResult = \case
    Left gs ->
      Guarded <$> traverse (\(a, b) -> (,) <$> goExpr a <*> goExpr b) gs
    Right a ->
      Unguarded <$> goExpr a

  goLetExpr = \case
    C.Var _ a -> do
      Left <$> getName a
    expr -> do
      n <- freshName
      expr' <- goExpr expr
      pure $ Right (optAnn . C.extractAnn $ expr, n, expr')

  letName = \case
    Left n -> n
    Right (_, n, _) -> n

  buildLet bs expr =
    case bs >>= toList of
      []  -> expr
      b : bs' -> do
        let
          ann1 = b ^. _3 . exprAnn
          ann2 = expr ^. exprAnn
          ann  = ann2 { annSpan = Pos.widenSourceSpan (annSpan ann1) (annSpan ann2) }
          bs'' = fmap (\(x, y, z) -> NonRec x y z) $ b : bs'
        Let ann bs'' expr

ctorToIdent :: N.ProperName 'N.ConstructorName -> N.Ident
ctorToIdent = N.Ident . N.runProperName

getQual :: [Text] -> N.Qualified a -> [Text]
getQual def = maybe def coerce . N.getQual

prim :: N.ModuleName
prim = N.ModuleName [N.ProperName "Prim"]

identUndefined :: N.Qualified N.Ident
identUndefined = N.Qualified (Just prim) (N.Ident "undefined")
