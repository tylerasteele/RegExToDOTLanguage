module Main where

import ParRegExp ( pRegExp, myLexer )
import System.Exit (die)
import AbsRegExp ( Ident(Ident), RegExp(..) ) 
import Control.Monad ()
import Control.Monad.State.Lazy (State, evalState, get, MonadState (put), modify, gets) 

data NFAst = NFAst { label :: Int , accept :: Bool}

main :: IO ()
main = do
  c <- getContents
  case pRegExp (myLexer c) of
    Left _ -> die "parse error! try testing with the bnfc generated modules (`make parse`)"
    Right tree -> putStrLn (start ++ compile tree ++ "\n}")

initialNFA :: NFAst
initialNFA = NFAst { label = 0, accept = True}

start :: String 
start = "digraph G \n\n{rankdir = LR ;\nfake [style = invisible] ; 0 [label = \"0\"]; \nfake -> 0\n\n"

compile :: RegExp -> String 
compile tree = evalState (compileM tree) initialNFA

 
compileM :: RegExp -> State NFAst String 
compileM (RSymbol reg) = do 
  freshM 
  st <- get 
  acc <- accState (accept st)
  return $ show (label st) ++ " [label = " ++ show (label st) ++ " shape = \"" ++ addShape acc ++ "\"];\n" ++ show (label st - 1) ++ "->" ++ show (label st) ++ " [label = \"" ++ getStrI reg ++ "\"];\n"


compileM  REmpty = do 
  freshM 
  st <- get 
  acc <- accState (accept st) 
  return $ show (label st) ++ " [label = " ++ show (label st) ++ " shape = \"" ++ addShape acc ++ "\"];\n" ++ show (label st - 1) ++ "->" ++ show (label st) ++ " [label = \"&#949;\"];\n"


compileM (RUnion r1 r2) = do 
  st <- get 
  acc <- accState (accept st)

  -- First Branch 
  br <- gets label 
  s1 <- compileM REmpty
  s2 <- compileM r1 
  rb <- gets label 

  freshM

  -- Second Branch
  bl <- gets label 
  let s3 = show br ++ "->" ++ show bl ++ " [label = \"&#949;\"];\n"
  s4 <- compileM r2 

  freshM
  
  -- Join 
  f <- gets label 
  let s5 = show rb ++ "->" ++ show f ++ " [label = \"&#949;\"];\n"
  let s6 = show (f - 1) ++ "->" ++ show f ++ " [label = \"&#949;\"];\n" ++ show f ++ "[shape = \"" ++ addShape acc ++ "\"];\n"

  return (s1 ++ s2 ++ s3 ++ s4 ++ s5 ++ s6)


compileM (RSequence r1 r2) = do 
  st <- get 
  acc <- accState (accept st)

  s1 <- compileM REmpty  
  s2 <- compileM r1 
  s3 <- compileM REmpty 
  s4 <- compileM r2 
  ac <- gets label
  return $ s1 ++ s2 ++ s3 ++ s4 ++ "\n"  ++ show ac ++ "[shape = \"" ++ addShape acc ++ "\"];\n"

compileM (RClosure r1) = do 
  st <- get 
  acc <- accState (accept st)

  -- Closure 
  s1 <- compileM r1
  s2 <- compileM REmpty 
  rb <- gets label 
  let s3 = show (rb - 1) ++ "->" ++ show (label st) ++ " [label = \"&#949;\"];\n"

  -- Empty case 
  let s4 = show (label st) ++ "->" ++ show rb ++ " [label = \"&#949;\"];\n" ++ show rb ++ "[shape = \"" ++ addShape acc ++ "\"];\n"
  return $ s1 ++ s2 ++ s3 ++ s4


-- Helpers --
freshM :: State NFAst () 
freshM = do
   modify (\st -> st { label = label st + 1})

getStrI :: Ident -> String 
getStrI (Ident r) = r

accState :: Bool -> State NFAst Bool 
accState x = do 
  let y = x
  modify (\st -> st { accept = False})
  return y

addShape :: Bool -> String 
addShape b 
  | b == False = "oval" 
  | b == True = "doublecircle" 