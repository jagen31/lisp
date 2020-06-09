module AST where

data AST = ANumb Int | ABool Bool | AStr String | AId String 
  | ALet [String] [AST] AST
  | AFun [String] AST 
  | ACall AST [AST]
  | ASyntax AST deriving Show
