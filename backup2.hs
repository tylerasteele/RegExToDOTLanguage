--module Main where

import ParRegExp ( pRegExp, myLexer )
import System.Exit (die)
import AbsRegExp ( Ident(Ident), RegExp(..) ) 
import Control.Monad ()
import Control.Monad.State.Lazy 

data NFAState = NFAState { f :: Int, b :: Int } 

main :: IO ()
main = do
  c <- getContents
  case pRegExp (myLexer c) of
    Left _ -> die "parse error! try testing with the bnfc generated modules (`make parse`)"
    Right tree -> putStrLn (start ++ compile tree ++ "\n}")


start :: String 
start = "digraph G \n\n{rankdir = LR ;\nfake [style = invisible] ; 0 [label = \"0\"]; \nfake -> 0\n\n"


compile :: RegExp -> String 
compile tree = evalState (compileM tree) inST


compileM :: RegExp -> State NFAState String
compileM (RSymbol reg) = do 
  freshM
  nST <- get 
  return $ "->" ++ show (f nST) ++ "[label = \"" ++ getStrI reg ++ "\"]\n"




  --" [label = " ++ show l ++ "] ;\n" ++ show (l - 1) ++ "->" ++ show l ++ "[label = " ++ getStrI reg ++ " ];\n")

compileM REmpty = do
  undefined 


compileM (RUnion r1 (RSymbol r) ) = do

-- Union
compileM (RUnion r1 r2) = do
  nST <- get

  

  x <- gets f 

  s1 <- compile

  s2 <- compile 

  return $ s1 ++ s2 



  let str1 = show (f nST) ++ "->"
  s1 <- compileM r1 
  let str2 = str1 ++ s1 ++ " aaaaaaaaaaaaaa "
  
  -- nST <- get 
  let str3 = show (f nST) ++ "->"
  s2 <- compileM r2 
  let str4 = str3 ++ s2 ++ " bbbbbbbbbbbbbbbb "


  return $ str2 ++ str4
-- compileM (RUnion r1 r2) = do
--   q0 <- get
--   s1 <- compileM r1 
--   s2 <- compileM r2 
--   return $ "\n" ++ show q0 ++ s1 ++ "\n" ++ show q0 ++ s2 ++ "\n"


-- Sequence 
compileM (RSequence r1 r2) = do
  nST <- get
  s1 <- compileM r1 
  nST <- get 
  s2 <- compileM r2 
  return $ "" ++ show (f nST - 1) ++ s1 ++ "\n" ++ show (f nST) ++ s2 ++ "" 

--      -- 


-- Closure 
compileM (RClosure reg) = do 
  r <- compileM reg
  s <- get

  return $ r ++ "\n" ++ show 1 ++ "->" ++ show 1 ++ "\n"
  
  
  -- let 
  -- start = "digraph G \n{rankdir = LR ; \nfake [style = invisible] ; 0 [label = \"0\"]; \nfake -> 0\n"
  -- in case regExp of
  --   RSymbol (Ident id) l -> start ++ "\n1 [label = \"1\"] ;\n0 -> 1 [label = " ++ id ++ " ];\n}"
  --   --RUnion r1 r2 -> 


getStrI :: Ident -> String 
getStrI (Ident r) = r



inST :: NFAState 
inST = NFAState { f = 0, b = 0 }



freshM :: State NFAState Int 
freshM = do
  st <- get 
  put (st {f = f st + 1})
  return $ f st









{- Why wouldn't this increment when called RSymbol from RSequence
incM :: State NFAState ()
incM = modify (\st -> st { f = f st + 1})
-}

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