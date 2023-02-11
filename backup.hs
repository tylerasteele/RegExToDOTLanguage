module Main where

import ParRegExp ( pRegExp, myLexer )
import System.Exit (die)
import AbsRegExp ( Ident(Ident), RegExp(..) ) 
import Control.Monad ()
import Control.Monad.State.Lazy 

data NFAState = NFAState { fresh :: Int, branch :: Int } 

main :: IO ()
main = do
  c <- getContents
  case pRegExp (myLexer c) of
    Left _ -> die "parse error! try testing with the bnfc generated modules (`make parse`)"
    Right tree -> putStrLn (start ++ compile tree ++ "\n}")


start :: String 
start = "digraph G \n\n{rankdir = LR ;\nfake [style = invisible] ; 0 [label = \"0\"]; \nfake -> 0\n\n"


compile :: RegExp -> String 
compile tree = evalState (compileM tree) 0


compileM :: RegExp -> State Int String
compileM (RSymbol reg) = do 
  o <- get
  l <- fresh 
  return $ "->" ++ show l ++ "[label = \"" ++ getStrI reg ++ "\"]\n"




  --" [label = " ++ show l ++ "] ;\n" ++ show (l - 1) ++ "->" ++ show l ++ "[label = " ++ getStrI reg ++ " ];\n")

compileM REmpty = do
  undefined 

-- Union
compileM (RUnion r1 r2) = do
  n <- get
  s1 <- compileM r1 
  s2 <- compileM r2 
  return $ "\n" ++ show n ++ s1 ++ "\n" ++ show n ++ s2  
-- compileM (RUnion r1 r2) = do
--   q0 <- get
--   s1 <- compileM r1 
--   s2 <- compileM r2 
--   return $ "\n" ++ show q0 ++ s1 ++ "\n" ++ show q0 ++ s2 ++ "\n"


-- Sequence 
compileM (RSequence r1 r2) = do
  n <- get
  s1 <- compileM r1 
  s2 <- compileM r2 
  return $ "\n" ++ show n ++ s1 ++ "\n" ++ show n ++ s2  



-- Closure 
compileM (RClosure reg) = do 
  r <- compileM reg
  s <- get

  return $ r ++ "\n" ++ show s ++ "->" ++ show s ++ "\n"
  
  
  -- let 
  -- start = "digraph G \n{rankdir = LR ; \nfake [style = invisible] ; 0 [label = \"0\"]; \nfake -> 0\n"
  -- in case regExp of
  --   RSymbol (Ident id) l -> start ++ "\n1 [label = \"1\"] ;\n0 -> 1 [label = " ++ id ++ " ];\n}"
  --   --RUnion r1 r2 -> 


getStrI :: Ident -> String 
getStrI (Ident r) = r


fresh :: State Int Int 
fresh = do
    l <- get
    put (l + 1) 
    return (l + 1)







{-
Symbol (e.g.) a {a}
Sequence A B {ab | a ∈JAK,b ∈JBK}
Union A | B JAK∪JBK
Closure A∗ {a1a2 ...an | ai ∈JAK,n ≥0}
Empty {} (empty string)

Union
Sequence
Closure
Empty: "&#949;""
Symbol
-}