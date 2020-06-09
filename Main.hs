module Main where

import System.IO
import Parser
import Eval

import Control.Monad.Reader

asRight :: (Show a) => Either a b -> b
asRight (Right b) = b
asRight (Left a) = error $ show a

plus :: [Val] -> Val
plus (VNumb n:rest) = VNumb (n + unwrap (plus rest))
  where unwrap (VNumb n) = n

plus [] = VNumb 0
plus (other:rest) = error $ "+: argument was not a number in: " <> show other

globalEnv :: Env
globalEnv = [("+", VPrim plus)]

main :: IO ()
main = do
  f <- openFile "test.txt" ReadMode
  c <- hGetContents f
  putStrLn $ show $ ((`runReader` globalEnv) . eval . (`runReader` []) . parseSexpr . asRight . parseText) c
