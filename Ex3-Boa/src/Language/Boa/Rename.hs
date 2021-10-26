{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The rename stage remap all he identifiers in the tree to have unique names.
-- It is done to avoid shadowing problems.

module Language.Boa.Rename (rename) where

import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.Reader ( MonadReader, Reader, runReader, ask, local )
import Control.Monad.Trans.Except (ExceptT)
import Data.Generics.Fixplate.Base (Attr, Ann (Ann), Mu (Fix))
import qualified Data.Map as M
import Data.Text (Text)
import Data.Generics.Fixplate.Traversals (topDownTransformM)
import qualified Data.Text as T

import Language.Boa.Tag (Tag(..))
import Language.Boa.Error ()
import Language.Boa.Syntax (ExprF (..))


-- The renaming environment: a mapping from and old name to a new name
type Env = M.Map Text Text

newtype RenamingM a = RenamingM (ExceptT Text (Reader Env) a)
  deriving (Functor, Applicative, Monad, MonadError Text, MonadReader Env)

runRenamingM :: Env -> RenamingM a -> Either Text a
runRenamingM env (RenamingM x) = flip runReader env $ runExceptT x

rename :: Attr ExprF Tag -> Either Text (Attr ExprF Tag)
rename = runRenamingM mempty . topDownTransformM doRename
  where
    doRename :: Attr ExprF Tag -> RenamingM (Attr ExprF Tag)
    doRename expr = case expr of
      Fix (Ann tag (EId nm)) -> do
        env <- ask
        case M.lookup nm env of
          Nothing -> throwError $ "Unexpected identifier during renaming: " <> nm
          Just nm' -> pure $ Fix $ Ann tag $ EId nm'
      Fix (Ann tag (ELet bs _)) ->
        let addTag s = s <> "$" <> T.pack (show $ unTag tag)
            addBindings m = foldr ((\s acc -> M.insert s (addTag s) acc) . fst) m bs
        in local addBindings $ pure expr
      _ -> pure expr
