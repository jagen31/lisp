module Types where

data Sexpr = List [Sexpr] | Sym String | Str String | Numb Int deriving Show

data AST = ANumb Int | ABool Bool | AStr String | AId String 
  | ALet [String] [AST] AST
  | AFun [String] AST 
  | ACall AST [AST] deriving Show

data Val = VNumb Int | VStr String | VBool Bool | VPrim ([Val] -> Val)
  | VFun [String] AST Env

type Env = [(String, Val)]

instance (Show Val) where
  show (VNumb n) = show n
  show (VStr s) = show s
  show (VBool b) = show b
  show (VPrim _) = "(λ (...) primitive)"
  show (VFun names _ env) = "(λ (" <> (show names) <> ") ... ) \n in: " <> show env
