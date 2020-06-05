module Parser (parseText, parseSexpr) where

import Text.Parsec
import Data.Either
import Types

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

parseSexpr :: Sexpr -> AST
parseSexpr (Numb n) = ANumb n
parseSexpr (Str s) = AStr s
parseSexpr (Sym "true") = ABool True
parseSexpr (Sym "false") = ABool False
parseSexpr (Sym s) = AId s
parseSexpr (List ((Sym "let"):more)) = parseLet more
parseSexpr (List ((Sym "fun"):more)) = parseFun more
parseSexpr (List (fun:args)) = ACall (parseSexpr fun) (map parseSexpr args)
parseSexpr (List []) = error "empty parens"

symToString :: Sexpr -> String
symToString (Sym s) = s
symToString bad = error $ "Not a symbol in " <> show bad

parseLet [(List bindings), body] = 
  let (names, exprs) = foldr trans ([],[]) bindings in
    ALet names exprs $ parseSexpr body
  where trans (List [n,e]) (ns,es) = ((symToString n):ns, (parseSexpr e):es)
        trans _ _ = error $ "invalid binding syntax in: " <> show bindings
parseLet other = error $ "invalid let syntax in: " <> show other

parseFun [List args, body] = AFun (map symToString args) $ parseSexpr body
parseFun _ = error "invalid fun syntax"
