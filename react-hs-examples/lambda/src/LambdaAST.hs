{-# LANGUAGE Strict #-}
module LambdaAST
  ( Name
  , Expr(..)
  , replace
  , getAnn
  , unAnno
  , ExprPath
  , collectPaths
  , collectPathsNormal
  -- , applyAt
  , ppExpr
  ) where


import qualified Data.Text as Text

-------------------------------------------------------------------------------
type Name = Text.Text


data Expr ann
  = Var !ann !Name
  | Abs !ann !Name !(Expr ann)
  | App !ann !(Expr ann) !(Expr ann)
  deriving (Eq, Show)


getAnn :: Expr ann -> ann
getAnn expr =
  case expr of
    Var ann _ -> ann
    App ann _ _ -> ann
    Abs ann _ _ -> ann


replace :: Name -> Expr ann -> Expr ann -> Expr ann
replace name value expr =
  case expr of
    Var _ n ->
      if name == n then value else expr
    Abs ann n b ->
      if name == n then expr else Abs ann n (replace name value b)
    App ann a b ->
      App ann (replace name value a) (replace name value b)


unAnno :: Expr a -> Expr ()
unAnno expr =
  case expr of
    Var _ name ->
      Var () name
    App _ a b ->
      App () (unAnno a) (unAnno b)
    Abs _ n b ->
      Abs () n (unAnno b)

-------------------------------------------------------------------------------
type ExprPath = [Bool]


collectPaths :: ExprPath -> Expr a -> [ExprPath]
collectPaths path expr =
  case expr of
    Var _ _ -> []
    Abs _ _ body -> collectPaths (True : path) body
    App _ fun@Abs{} arg ->
      path : collectPaths (False : path) fun ++ collectPaths (True : path) arg
    App _ fun arg -> collectPaths (False : path) fun ++ collectPaths (True : path) arg


collectPathsNormal :: ExprPath -> Expr a -> [ExprPath]
collectPathsNormal path expr =
  case expr of
    Var _ _ -> []
    Abs _ _ _ -> []
    App _ fun@Abs{} arg ->
      path : collectPathsNormal (False : path) fun ++ collectPathsNormal (True : path) arg
    App _ fun arg -> collectPathsNormal (False : path) fun ++ collectPathsNormal (True : path) arg


--applyAt :: Monad m => ExprPath -> (Expr ann -> m (Expr ann)) -> Expr ann -> m (Expr ann)
--applyAt path f expr =
--  case (path, expr) of
--    ([], e) ->
--      f e
--    (False:path', App ann a b) ->
--      App ann <$> applyAt path' f a <*> pure b
--    (True:path', App ann a b) ->
--      App ann <$> pure a <*> applyAt path' f b
--    (True:path', Abs ann name b) ->
--      Abs ann name <$> applyAt path' f b
--    _ ->
--      return expr

-------------------------------------------------------------------------------
ppExpr :: Expr a -> String
ppExpr e = case e of
  Var _ x -> Text.unpack x
  Abs _ x b -> "\\" ++ Text.unpack x ++ "." ++ ppExpr b
  App _ a b -> "@" ++ ppExpr a ++ " " ++ ppExpr b


