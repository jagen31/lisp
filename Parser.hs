module Parser (parseText, parseSexpr) where

import Eval
import AST

import Text.Parsec
import Data.Either
import Control.Monad.Reader

data Sexpr = List [Sexpr] | Sym String | Str String | Numb Int deriving Show

lispP :: Parsec String st Sexpr
lispP = listP <|> symP <|> stringP <|> numberP

listP :: Parsec String st Sexpr
listP = do 
  (char '(') 
  result <- manyTill (do { result <- lispP; skipMany space; return result }) 
                     (char ')')
  return $ List result

stringP :: Parsec String st Sexpr
stringP = do
  char '"'
  result <- manyTill anyChar (char '"')
  return $ Str result

symP :: Parsec String st Sexpr
symP = do
  first <- letter <|> oneOf "+-*/._-"
  rest <- many alphaNum
  return $ Sym (first:rest)

numberP :: Parsec String st Sexpr
numberP = do
  result <- many digit
  return $ Numb $ read result

parseText :: String -> Either ParseError Sexpr
parseText = parse (do {result <- lispP ; eof ; return result}) "Parse Error"

parseSexpr :: Sexpr -> Reader Env AST
parseSexpr (Numb n) = return $ ANumb n
parseSexpr (Str s) = return $ AStr s
parseSexpr (Sym "true") = return $ ABool True
parseSexpr (Sym "false") = return $ ABool False
parseSexpr (Sym s) = return $ AId s
parseSexpr (List ((Sym "let-syntax"):more)) = parseLetSyntax more
parseSexpr (List ((Sym "let"):more)) = parseLet more
parseSexpr (List ((Sym "fun"):more)) = parseFun more
parseSexpr (List (fun:args)) = do 
  fun' <- parseSexpr fun
  args' <- sequence (map parseSexpr args)
  return $ ACall fun' args'
parseSexpr (List []) = error "empty parens"

symToString :: Sexpr -> String
symToString (Sym s) = s
symToString bad = error $ "Not a symbol in " <> show bad

trans :: [Sexpr] -> ([String], [Reader Env AST])
trans li = foldr go ([], []) li
  where go (List [n,e]) (ns,es) = ((symToString n):ns, (parseSexpr e):es)

parseLet :: [Sexpr] -> Reader Env AST
parseLet [List bindings, body] = do
  let (names, exprs) = trans bindings
  body' <- parseSexpr body
  env <- ask
  return $ ALet names (map (`runReader` env) exprs) body'
parseLet other = error $ "invalid let syntax in: " <> show other

parseLetSyntax :: [Sexpr] -> Reader Env AST
parseLetSyntax [List bindings, body] = do
  let (names, exprs) = trans bindings
  env <- ask
  local (++ (zip names $ runReader (sequence (map (>>= eval) exprs)) env)) $ parseSexpr body
parseLetSyntax other = error $ "invlalid let-syntax syntax in: " <> show other

parseFun :: [Sexpr] -> Reader Env AST
parseFun [List args, body] = do 
  body' <- parseSexpr body
  return $ AFun (map symToString args) body'
parseFun _ = error "invalid fun syntax"
