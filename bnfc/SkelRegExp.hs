-- File generated by the BNF Converter (bnfc 2.9.3).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module SkelRegExp where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified AbsRegExp

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: AbsRegExp.Ident -> Result
transIdent x = case x of
  AbsRegExp.Ident string -> failure x

transRegExp :: AbsRegExp.RegExp -> Result
transRegExp x = case x of
  AbsRegExp.REmpty -> failure x
  AbsRegExp.RUnion regexp1 regexp2 -> failure x
  AbsRegExp.RSequence regexp1 regexp2 -> failure x
  AbsRegExp.RClosure regexp -> failure x
  AbsRegExp.RSymbol ident -> failure x
