{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module ExprToAsm (compileExpr, compileProg) where

import Asm
import PPAsm (asmToStr)
import Syntax

import Control.Monad.Except
import Control.Monad.State
import Data.Traversable (for)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)


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

emptyEnv :: Env
emptyEnv = M.empty

addVar :: Text -> Env -> (Env, Offset)
addVar var env =
  let off = M.size env + 1
  in (M.insert var off env, off)

lookupVar :: Text -> Env -> Maybe Offset
lookupVar = M.lookup

compileExpr :: Expr a -> Either Text Text
compileExpr = fmap asmToStr . runCodeGenM mempty . doCompileExpr

doCompileExpr :: Expr a -> CodeGenM [Instruction]
doCompileExpr = \case
  ENumber _ n -> pure [IMov (AReg RAX) (ALit n)]
  EIden _ var -> do
    env <- get
    case lookupVar var env of
      Just off -> pure
        [IMov (AReg RAX) (ARegOffset RSP (-off))]
      Nothing -> throwError $ T.pack $ printf
        "Error: cannot find variable %s in the envirnoment" var
  EPrim1 _ PAdd1 e ->
    (<> [IAdd (AReg RAX) (ALit 1)]) <$> doCompileExpr e
  EPrim1 _ PSub1 e ->
    (<> [IAdd (AReg RAX) (ALit (-1))]) <$> doCompileExpr e
  ELet _ binds inExpr -> do
    bindsCode <- for binds $ \(var, e) -> do
      bindCode <- doCompileExpr e
      env <- get
      let (env', off) = addVar var env
      put env'
      pure $ bindCode <> [IMov (ARegOffset RSP (-off)) (AReg RAX)]
    inCode <- doCompileExpr inExpr
    pure $ concat bindsCode <> inCode


compileProg :: Expr a -> Either Text Text
compileProg e = do
  body <- compileExpr e
  pure $ T.intercalate "\n"
    [ prelude
    , body
    , suffix
    ]
