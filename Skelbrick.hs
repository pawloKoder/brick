module Skelbrick where

-- Haskell module generated by the BNF converter

import Absbrick
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transCIdent :: CIdent -> Result
transCIdent x = case x of
  CIdent str  -> failure x


transProgram :: Program -> Result
transProgram x = case x of
  Progr fundeclarations  -> failure x


transStm :: Stm -> Result
transStm x = case x of
  SIf exp stms  -> failure x
  SIfElse exp stms1 stms2  -> failure x
  SJump jump_stm  -> failure x
  SExp exp  -> failure x


transJump_stm :: Jump_stm -> Result
transJump_stm x = case x of
  SjumpReturn  -> failure x
  SjumpReturnV exp  -> failure x
  SjumpBreak  -> failure x
  SjumpBreakV exp  -> failure x
  SjumpContinue  -> failure x
  SjumpContinueV exp  -> failure x


transExp :: Exp -> Result
transExp x = case x of
  ENone  -> failure x
  EAsign cident exp  -> failure x
  EYield exp  -> failure x
  ETrue  -> failure x
  EFalse  -> failure x
  EFor cident exp1 exp2 stms3  -> failure x
  EWhile exp stms  -> failure x
  EFunDef fundeclaration  -> failure x
  EInt n  -> failure x
  EString str  -> failure x
  EIdent cident  -> failure x
  EFunPar exp exps  -> failure x


transFunDeclaration :: FunDeclaration -> Result
transFunDeclaration x = case x of
  FunDec cident stms  -> failure x



