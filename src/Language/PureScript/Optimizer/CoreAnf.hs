module Language.PureScript.Optimizer.CoreAnf where

import Prelude

import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Text.Prettyprint.Doc as D
import qualified Language.PureScript.AST.Literals as L
import qualified Language.PureScript.Names as N
import Language.PureScript.PSString (PSString)

type Name = (N.ModuleName, Integer)

data Module a = Module
  { modName :: N.ModuleName
  , modDeps :: [N.ModuleName]
  , modDecls :: M.Map N.Ident (Name, Decl a)
  , modSort :: [N.Ident]
  } deriving (Show, Functor)

data Decl a
  = DeclCtor Int
  | DeclExpr [Name] (Expr a)
  | DeclForeign N.Ident
  deriving (Show, Functor)

data Expr a
  = Lit a (L.Literal Name)
  | Let a Name (Expr a) (Expr a)
  | LetRec [((a, Name), Expr a)] (Expr a)
  | Abs a [Name] (Expr a)
  | App a Name [Name]
  | Var a Name
  | Access a Name PSString
  | Update a Name [(PSString, Name)]
  | Case a [Name] [CaseAlternative a]
  deriving (Show, Functor)

data CaseAlternative a = CaseAlternative
  { caltBinders :: [Binder a]
  , caltResult :: Guarded a
  } deriving (Show, Functor)

data Guarded a
  = Unguarded (Expr a)
  | Guarded [(Expr a, Expr a)]
  deriving (Show, Functor)

data Binder a
  = BinderWildcard a
  | BinderLit a (L.Literal (Binder a))
  | BinderVar a Name
  | BinderNamed a Name (Binder a)
  | BinderCtor a Name [Binder a]
  deriving (Show, Functor)

ppModule :: Module a -> D.Doc void
ppModule (Module {..}) =
  D.vsep $
    [ "module" D.<+> D.pretty (N.runModuleName modName)
    , "import"
    , D.indent 2
        . D.vsep
        . fmap (D.pretty . N.runModuleName)
        $ modDeps
    , "export"
    , D.indent 2
        . D.vsep
        . fmap (ppExport . fmap fst)
        . M.toList
        $ modDecls
    ] <> decls
  where
  decls =
    fmap (uncurry (ppDecl modName))
      . mapMaybe (flip M.lookup modDecls)
      $ modSort

  ppExport (ident, n) =
    D.pretty (N.runIdent ident)
      <> " "
      <> ppName modName n

ppDecl :: N.ModuleName -> Name -> Decl a -> D.Doc void
ppDecl mn n = \case
  DeclCtor len ->
    D.hsep
      [ ppName mn n
      , "="
      , "constructor"
      , D.pretty len
      ]
  DeclForeign ident ->
    D.hsep
      [ ppName mn n
      , "="
      , "foreign"
      , D.pretty $ show $ N.runIdent ident
      ]
  DeclExpr group expr ->
    D.vsep
      [ ppName mn n <> ppRec group D.<+> "="
      , D.indent 2
          . ppExpr mn
          $ expr
      ]

ppExpr :: N.ModuleName -> Expr a -> D.Doc void
ppExpr mn = \case
  Lit _ x ->
    ppLit
      . fmap (ppName mn)
      $ x
  Let _ n a b ->
    D.vsep
      [ ppName mn n D.<+> "="
      , D.indent 2
          . ppExpr mn
          $ a
      , ppExpr mn b
      ]
  LetRec bs a -> do
    let
      group = snd . fst <$> bs
      ppBinding ((_, n), expr) =
        D.vsep
          [ ppName mn n <> ppRec group D.<+> "="
          , D.indent 2
              . ppExpr mn
              $ expr
          ]
    D.vsep
      [ D.vsep
          . fmap ppBinding
          $ bs
      , ppExpr mn a
      ]
  Abs _ as b ->
    D.vsep
      [ "\\" <> D.hsep (ppName mn <$> as) D.<+> "->"
      , D.indent 2
          . ppExpr mn
          $ b
      ]
  App _ a bs ->
    D.lparen
      <> D.hsep (ppName mn a : (ppName mn <$> bs))
      <> D.rparen
  Var _ n ->
    ppName mn n
  Access _ a b ->
    ppName mn a <> "." <> D.pretty (show b)
  Update _ a pairs ->
    ppName mn a <> "." <> do
      D.encloseSep D.lbrace D.rbrace D.comma
        . fmap (\(k, v) -> D.pretty (show k) D.<+> D.equals D.<+> ppName mn v)
        $ pairs
  Case _ ns alts -> do
    D.vsep
      [ "case" D.<+> D.hsep (ppName mn <$> ns) D.<+> "of"
      , D.indent 2
          . D.vsep
          . fmap (ppCaseAlternative mn)
          $ alts
      ]

ppCaseAlternative :: N.ModuleName -> CaseAlternative a -> D.Doc void
ppCaseAlternative mn (CaseAlternative bs res) =
  case res of
    Guarded gs ->
      D.vsep
        [ bs'
        , D.indent 2
            . D.vsep
            . fmap ppGuard
            $ gs
        ]
    Unguarded expr ->
      D.vsep
        [ bs' D.<+> "->"
        , D.indent 2
            . ppExpr mn
            $ expr
        ]
  where
  bs' =
    D.hsep
      . fmap (ppBinder mn)
      $ bs

  ppGuard (a, b) =
    D.vsep
      [ "|" D.<+> D.align (ppExpr mn a) D.<+> "->"
      , D.indent 4
          . ppExpr mn
          $ b
      ]

ppBinder :: N.ModuleName -> Binder a -> D.Doc void
ppBinder mn = \case
  BinderWildcard _ ->
    "_"
  BinderLit _ x ->
    ppLit
      . fmap (ppBinder mn)
      $ x
  BinderVar _ x ->
    ppName mn x
  BinderNamed _ a b ->
    D.lparen
      <> ppName mn a
      D.<+> "="
      D.<+> ppBinder mn b
      <> D.rparen
  BinderCtor _ a bs ->
    D.lparen
      <> D.hsep (ppName mn a : (ppBinder mn <$> bs))
      <> D.rparen

ppLit :: L.Literal (D.Doc void) -> D.Doc void
ppLit = \case
  L.ArrayLiteral x ->
    D.encloseSep D.lbracket D.rbracket D.comma x
  L.ObjectLiteral pairs ->
    D.encloseSep D.lbrace D.rbrace D.comma
      . fmap (\(k, v) -> D.pretty (show k) D.<+> D.equals D.<+> v)
      $ pairs
  L.NumericLiteral x ->
    either D.pretty D.pretty x
  L.StringLiteral x ->
    D.pretty $ show x
  L.CharLiteral x ->
    D.pretty $ show x
  L.BooleanLiteral x ->
    if x then "true" else "false"

ppName :: N.ModuleName -> Name -> D.Doc void
ppName mn (mn', n)
  | mn == mn' = "@" <> D.pretty n
  | otherwise = D.pretty (N.runModuleName mn') <> "@" <> D.pretty n

ppRec :: [Name] -> D.Doc void
ppRec group
  | [] <- group = mempty
  | otherwise =
      D.lbracket
        <> D.hsep (D.punctuate D.comma (("@" <>) . D.pretty . snd <$> group))
        <> D.rbracket
