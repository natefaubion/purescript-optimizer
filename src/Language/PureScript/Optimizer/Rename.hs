module Language.PureScript.Optimizer.Rename where

import Prelude

import Control.Monad (join)
import qualified Control.Monad.State.Class as MS
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.List.NonEmpty as NE
import Data.Foldable (fold)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, catMaybes)
import qualified Language.PureScript.CoreFn as C
import qualified Language.PureScript.Names as N
import Language.PureScript.Optimizer.CoreAnf

type Scope = M.Map (N.Qualified N.Ident) (NE.NonEmpty Name)

data RenameState = RenameState
  { rsModuleName :: N.ModuleName
  , rsFresh :: Integer
  , rsScope :: Scope
  }

freshName :: MS.MonadState RenameState m => m Name
freshName = MS.state $ \st@(RenameState {..}) ->
  ((rsModuleName, rsFresh), st { rsFresh = rsFresh + 1 })

bindName :: MS.MonadState RenameState m => N.Qualified N.Ident -> m Name
bindName ident = MS.state $ \st@(RenameState {..}) -> do
  let
    name = (fromMaybe rsModuleName (N.getQual ident), rsFresh)
    alterFn = \case
      Nothing -> Just $ pure name
      Just ne -> Just $ NE.cons name ne
    scope =
      case N.getQual ident of
        Nothing ->
          M.alter alterFn ident
           . M.alter alterFn (N.Qualified (Just rsModuleName) . N.disqualify $ ident)
           $ rsScope
        Just _ ->
          M.alter alterFn ident rsScope
  (name, st { rsFresh = rsFresh + 1, rsScope = scope })

bindName' :: MS.MonadState RenameState m => N.Ident -> m Name
bindName' = bindName . N.Qualified Nothing

assocName :: MS.MonadState RenameState m => N.Qualified N.Ident -> Name -> m ()
assocName ident name = MS.state $ \st@(RenameState {..}) -> do
  let
    alterFn = \case
      Nothing -> Just $ pure name
      Just ne -> Just $ NE.cons name ne
    scope =
      case N.getQual ident of
        Nothing ->
          M.alter alterFn ident
           . M.alter alterFn (N.Qualified (Just rsModuleName) . N.disqualify $ ident)
           $ rsScope
        Just _ ->
          M.alter alterFn ident rsScope
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
    Just ne -> NE.head ne

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

renameModule :: M.Map N.ModuleName (Module C.Ann) -> C.Module C.Ann -> Module C.Ann
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
    RenameState moduleName 0 (scope <> M.singleton identUndefined (pure (prim, 0)))

  scope =
    fold
      . catMaybes
      . fmap (fmap scopeFn . flip M.lookup env . snd)
      $ moduleImports

  nameFn mn (k, v) =
    (N.Qualified (Just mn) k, pure $ fst v)

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

renameExpr :: MS.MonadState RenameState m => C.Expr C.Ann -> m (Expr C.Ann)
renameExpr = goExpr
  where
  goExpr = scoped . \case
    C.Literal ann a -> do
      goLit ann a
    C.Accessor ann p expr -> scoped $ do
      expr' <- goLetExpr expr
      pure $ buildLet expr' $ Access ann (letName expr') p
    C.ObjectUpdate ann expr pairs -> do
      expr' <- goLetExpr expr
      pairs' <- traverse (traverse goLetExpr) pairs
      pure
       . buildLet expr'
       . foldr (buildLet . snd) (Update ann (letName expr') . fmap (fmap letName) $ pairs')
       $ pairs'
    C.Abs ann n expr -> do
      n' <- bindName' n
      Abs ann [n'] <$> goExpr expr
    C.App ann a b -> do
      a' <- goLetExpr a
      b' <- goLetExpr b
      pure
       . buildLet a'
       . buildLet b'
       $ App ann (letName a') [letName b']
    C.Let _ bs expr -> do
      bs' <- catMaybes <$> traverse goBind bs
      expr' <- goExpr expr
      let
        letFn b next = case b of
          Left (ann, n, x) -> Let ann n x next
          Right rs -> LetRec rs next
      pure $ foldr letFn expr' bs'
    C.Case ann exprs alts -> do
      exprs' <- traverse goLetExpr exprs
      alts' <- traverse goAlt alts
      pure $ foldr buildLet (flip (Case ann) alts' . fmap letName $ exprs') exprs'
    C.Var ann v ->
      Var ann <$> getName v
    C.Constructor _ _ ident _ ->
      error $ "renameExpr: Invalid constructor " <> show ident

  goLit ann = \case
    C.ArrayLiteral exprs -> scoped $ do
      exprs' <- traverse goLetExpr exprs
      pure $ foldr buildLet (Lit ann . C.ArrayLiteral . fmap letName $ exprs') exprs'
    C.ObjectLiteral pairs -> scoped $ do
      pairs' <- traverse (traverse goLetExpr) pairs
      pure $ foldr (buildLet . snd) (Lit ann . C.ObjectLiteral . fmap (fmap letName) $ pairs') pairs'
    C.NumericLiteral x ->
      pure $ Lit ann (C.NumericLiteral x)
    C.StringLiteral x ->
      pure $ Lit ann (C.StringLiteral x)
    C.CharLiteral x ->
      pure $ Lit ann (C.CharLiteral x)
    C.BooleanLiteral x ->
      pure $ Lit ann (C.BooleanLiteral x)

  goBind = \case
    C.NonRec _ a (C.Var _ b) -> do
      n <- getName b
      assocName' a n
      pure $ Nothing
    C.NonRec ann ident expr -> do
      n <- bindName' ident
      expr' <- goExpr expr
      pure $ Just $ Left (ann, n, expr')
    C.Rec bs -> do
      let
        bindFn ((ann, ident), expr) =
          (,expr) . (ann,) <$> bindName' ident
      bs' <- traverse bindFn bs
      bs'' <- traverse (traverse goExpr) bs'
      pure $ Just $ Right bs''

  goAlt (C.CaseAlternative bs res) =
    scoped $ CaseAlternative
      <$> traverse goBinder bs
      <*> goResult res

  goBinder = \case
    C.NullBinder ann ->
      pure $ BinderWildcard ann
    C.LiteralBinder ann lit ->
      BinderLit ann <$> goBinderLit lit
    C.VarBinder ann ident ->
      BinderVar ann <$> bindName' ident
    C.NamedBinder ann ident b ->
      BinderNamed ann <$> bindName' ident <*> goBinder b
    C.ConstructorBinder ann _ ctor args ->
      BinderCtor ann <$> getName (ctorToIdent <$> ctor) <*> traverse goBinder args

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
      pure $ Right (C.extractAnn expr, n, expr')

  letName = \case
    Left n -> n
    Right (_, n, _) -> n

  buildLet = \case
    Left _ -> id
    Right (ann, n, rhs) -> Let ann n rhs

ctorToIdent :: N.ProperName 'N.ConstructorName -> N.Ident
ctorToIdent = N.Ident . N.runProperName

prim :: N.ModuleName
prim = N.ModuleName [N.ProperName "Prim"]
identUndefined :: N.Qualified N.Ident
identUndefined = N.Qualified (Just prim) (N.Ident "undefined")
