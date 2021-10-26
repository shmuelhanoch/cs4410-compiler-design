
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Boa.ExprToAsm (compileExpr, compileProg) where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (MonadState, State, evalState, get, put)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)
import Text.Printf (printf)
import Data.Generics.Fixplate (Attr, Mu (Fix), Ann (Ann))

import Language.Boa.Asm
import Language.Boa.PPAsm
import Language.Boa.Syntax (ExprF (..), Prim1 (Add1, Sub1), Prim2 (Add, Sub, Times))

type Env = Map Text Offset

newtype CodeGenM a = CodeGenM (ExceptT Text (State Env) a)
  deriving (Functor, Applicative, Monad, MonadError Text, MonadState Env)

runCodeGenM :: Env -> CodeGenM a -> Either Text a
runCodeGenM env (CodeGenM x) = flip evalState env $ runExceptT x

prelude :: Text
prelude =  T.intercalate "\n"
  [ "section .text"
  , "global program_entry_point"
  , "program_entry_point:"
  ]

suffix :: Text
suffix = "ret"

addVar :: Text -> Env -> (Env, Offset)
addVar var env =
  let off = M.size env + 1
  in (M.insert var off env, off)

lookupVar :: Text -> Env -> Maybe Offset
lookupVar = M.lookup

compileExpr :: Attr ExprF () -> Either Text Text
compileExpr = fmap asmToStr . runCodeGenM mempty . doCompileExpr

doCompileExpr :: Attr ExprF () -> CodeGenM [Instruction]
doCompileExpr = \case
  Fix (Ann _ (ENumber n)) -> pure [IMov (AReg RAX) (ALit n)]
  Fix (Ann _ (EId var)) -> do
    env <- get
    case lookupVar var env of
      Just off -> pure
        [IMov (AReg RAX) (ARegOffset RSP (-off))]
      Nothing -> throwError $ "Variable not in scope: " <> var
  Fix (Ann _ (EPrim1 Add1 e)) ->
    (<> [IAdd (AReg RAX) (ALit 1)]) <$> doCompileExpr e
  Fix (Ann _ (EPrim1 Sub1 e)) ->
    (<> [IAdd (AReg RAX) (ALit (-1))]) <$> doCompileExpr e
  Fix (Ann _ (EPrim2 op e1 e2)) -> compileBinOp op e1 e2
  Fix (Ann _ (ELet binds inExpr)) -> do
    bindsCode <- for binds $ \(var, e) -> do
      bindCode <- doCompileExpr e
      env <- get
      let (env', off) = addVar var env
      put env'
      pure $ bindCode <> [IMov (ARegOffset RSP (-off)) (AReg RAX)]
    inCode <- doCompileExpr inExpr
    pure $ concat bindsCode <> inCode
  Fix (Ann _ (EIf _cond _thenE _elseE)) -> undefined

compileBinOp :: Prim2 -> Attr ExprF () -> Attr ExprF () -> CodeGenM [Instruction]
compileBinOp op e1 e2 = do
  let op' = opToAsm op
  e1' <- doCompileExpr e1
  e2' <- case e2 of
    Fix (Ann _ (ENumber n)) -> pure [op' (AReg RAX) (ALit n)]
    Fix (Ann _ (EId var)) -> do
      env <- get
      case lookupVar var env of
        Just off -> pure
          [op' (AReg RAX) (ARegOffset RSP (-off))]
        Nothing -> throwError $ "Variable not in scope: " <> var
    _ -> throwError $ T.pack $ printf
      "The expression is not in ANF: got %s as an argument to Sub."
      (show e2)
  pure $ e1' <> e2'
  where
    opToAsm = \case
      Add -> IAdd
      Sub -> ISub
      Times -> IMul


compileProg :: Attr ExprF () -> Either Text Text
compileProg e = do
  body <- compileExpr e
  pure $ T.intercalate "\n"
    [ prelude
    , body
    , suffix
    ]
