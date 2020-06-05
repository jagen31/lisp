module Eval where

import Types

eval :: Env -> AST -> Val
eval env expr = eval' expr
  where eval' (ANumb i) = VNumb i
        eval' (ABool b) = VBool b
        eval' (AStr s) = VStr s
        eval' (AId i) = 
          case (lookup i env) of
            (Just v) -> v
            otherwise -> error $ "Unbound identifier in: " <> i
        eval' (ALet names exprs body) = 
          eval ((zip names (map eval' exprs)) ++ env) body
        eval' (AFun names body) = VFun names body env
        eval' (ACall fun args) =
          case evald of
            (VFun names body env) -> eval ((zip names evaldArgs) ++ env) body
            (VPrim fn) -> fn evaldArgs
            otherwise -> error $ "Call with non-fun in: " <> show evald
          where evald = eval' fun
                evaldArgs = map eval' args
