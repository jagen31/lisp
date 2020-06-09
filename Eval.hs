module Eval where

import Control.Monad.Reader
import AST

data Val = VNumb Int | VStr String | VBool Bool | VPrim ([Val] -> Val)
  | VFun [String] AST Env

type Env = [(String, Val)]

instance (Show Val) where
  show (VNumb n) = show n
  show (VStr s) = show s
  show (VBool b) = show b
  show (VPrim _) = "(λ (...) primitive)"
  show (VFun names _ env) = "(λ (" <> (show names) <> ") ... ) \n in: " <> show env

eval :: AST -> Reader Env Val
eval (ANumb i) = return $ VNumb i
eval (ABool b) = return $ VBool b
eval (AStr s) = return $ VStr s
eval (AId i) = do 
  val <- asks $ lookup i
  return $ case val of
            Just v -> v
            otherwise -> error $ "Unbound identifier in: " <> i
eval (ALet names exprs body) = do
  evald <- mapM eval exprs
  local ((zip names evald) ++) $ eval body
eval (AFun names body) = ask >>= \e -> return $ VFun names body e
eval (ACall fun args) = do 
  fun' <- eval fun
  args' <- mapM eval args
  case fun' of
    (VFun names body env) -> local ((zip names args') ++) $ eval body
    (VPrim fn) -> return $ fn args'
    otherwise -> error $ "Call with non-fun in: " <> show fun'
