{-# LANGUAGE Strict #-}
module LambdaFBAnn
  ( FBAnn(..)
  , annoFBAnn
  , annoFBAnnNonRecursive
  , applyAtAndAnno
  , renameConflicting
  , renameLambdas
  ) where


import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Set (Set)


import LambdaAST

-------------------------------------------------------------------------------
data FBAnn = FBAnn
  { freeAnn :: !(Set Name)
  , boundAnn :: !(Set Name)
  , conflictAnn :: !(Set Name)
  } deriving (Eq, Show)


instance Monoid FBAnn where
  mappend (FBAnn free1 bound1 _) (FBAnn free2 bound2 _) =
    FBAnn (Set.union free1 free2) (Set.union bound1 bound2) Set.empty
  mempty = FBAnn Set.empty Set.empty Set.empty


annoFBAnn :: Expr a -> Expr FBAnn
annoFBAnn expr =
  case expr of
    Var _ name ->
      Var (FBAnn (Set.singleton name) Set.empty Set.empty) name
    App _ a b ->
      let
        a' = annoFBAnn a
        b' = annoFBAnn b
      in
        case a' of
          Abs _ name body ->
            let
              FBAnn freeInArg _ _ = getAnn b'
              FBAnn _ boundInBody _ = getAnn body
              conflicting = Set.intersection (Set.delete name freeInArg) boundInBody
            in
              App ((mappend (getAnn a') (getAnn b')) { conflictAnn = conflicting }) a' b'
          _ ->
            App (mappend (getAnn a') (getAnn b')) a' b'
    Abs _ n b ->
      let
        b' = annoFBAnn b
        FBAnn free bound _ = getAnn b'
      in
        Abs (FBAnn (Set.delete n free) (Set.insert n bound) Set.empty) n b'


annoFBAnnNonRecursive :: Expr FBAnn -> Expr FBAnn
annoFBAnnNonRecursive expr =
  case expr of
    Var _ name ->
      Var (FBAnn (Set.singleton name) Set.empty Set.empty) name
    App _ a' b' ->
      case a' of
        Abs _ name body ->
          let
            FBAnn freeInArg _ _ = getAnn b'
            FBAnn _ boundInBody _ = getAnn body
            conflicting = Set.intersection (Set.delete name freeInArg) boundInBody
          in
            App ((mappend (getAnn a') (getAnn b')) { conflictAnn = conflicting }) a' b'
        _ ->
          App (mappend (getAnn a') (getAnn b')) a' b'
    Abs _ n b' ->
      let
        FBAnn free bound _ = getAnn b'
      in
        Abs (FBAnn (Set.delete n free) (Set.insert n bound) Set.empty) n b'


applyAtAndAnno :: Monad m => ExprPath -> (Expr FBAnn -> m (Expr FBAnn)) -> Expr FBAnn -> m (Expr FBAnn)
applyAtAndAnno path f expr =
  case (path, expr) of
    ([], e) ->
      annoFBAnn <$> f e
    (False:path', App ann a b) ->
      annoFBAnnNonRecursive <$> (App ann <$> applyAtAndAnno path' f a <*> pure b)
    (True:path', App ann a b) ->
      annoFBAnnNonRecursive <$> (App ann <$> pure a <*> applyAtAndAnno path' f b)
    (True:path', Abs ann name b) ->
      annoFBAnnNonRecursive <$> (Abs ann name <$> applyAtAndAnno path' f b)
    _ ->
      return expr


renameConflicting :: Map Name Name -> Expr FBAnn -> Expr FBAnn
renameConflicting nameMap expr =
  case expr of
    Var ann name ->
      maybe expr (Var ann) $ Map.lookup name nameMap
    Abs ann name body ->
      let
        name' = fromMaybe name $ Map.lookup name nameMap
        body' = renameConflicting nameMap body
      in Abs ann name' body'
    App ann a b ->
      App ann (renameConflicting nameMap a) (renameConflicting nameMap b)


renameLambdas :: Map Name Name -> Expr FBAnn -> Expr FBAnn
renameLambdas nameMap expr =
  case expr of
    Var ann name ->
      Var ann name
    App ann a b ->
      App ann (renameLambdas nameMap a) (renameLambdas nameMap b)
    Abs ann name body ->
      case Map.lookup name nameMap of
        Nothing ->
          Abs ann name $ renameLambdas nameMap body
        Just name' ->
          Abs ann name' $ renameConflicting nameMap body
