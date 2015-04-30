-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Parbrick where
import Absbrick
import Lexbrick
import ErrM

}

%name pProgram Program

-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype { Token }

%token 
 '-' { PT _ (TS _ 1) }
 '@' { PT _ (TS _ 2) }
 'Break' { PT _ (TS _ 3) }
 'Continue' { PT _ (TS _ 4) }
 'Def' { PT _ (TS _ 5) }
 'Else' { PT _ (TS _ 6) }
 'False' { PT _ (TS _ 7) }
 'For' { PT _ (TS _ 8) }
 'If' { PT _ (TS _ 9) }
 'Let' { PT _ (TS _ 10) }
 'Return' { PT _ (TS _ 11) }
 'True' { PT _ (TS _ 12) }
 'While' { PT _ (TS _ 13) }
 'Yield' { PT _ (TS _ 14) }
 '[' { PT _ (TS _ 15) }
 ']' { PT _ (TS _ 16) }

L_integ  { PT _ (TI $$) }
L_quoted { PT _ (TL $$) }
L_CIdent { PT _ (T_CIdent $$) }
L_err    { _ }


%%

Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }
String  :: { String }  : L_quoted {  $1 }
CIdent    :: { CIdent} : L_CIdent { CIdent ($1)}

Program :: { Program }
Program : ListFunDeclaration { Progr (reverse $1) } 


Stm :: { Stm }
Stm : '[' 'If' Exp ListStm ']' { SIf $3 (reverse $4) } 
  | '[' 'If' Exp ListStm ']' '[' 'Else' ListStm ']' { SIfElse $3 (reverse $4) (reverse $8) }
  | JumpStm { SJump $1 }
  | Exp { SExp $1 }
  | FunDeclaration { SFunDef $1 }


ListStm :: { [Stm] }
ListStm : {- empty -} { [] } 
  | ListStm Stm { flip (:) $1 $2 }


JumpStm :: { JumpStm }
JumpStm : '[' 'Return' ']' { SjumpReturn } 
  | '[' 'Return' Exp ']' { SjumpReturnV $3 }
  | '[' 'Break' ']' { SjumpBreak }
  | '[' 'Break' Exp ']' { SjumpBreakV $3 }
  | '[' 'Continue' ']' { SjumpContinue }
  | '[' 'Continue' Exp ']' { SjumpContinueV $3 }


Exp :: { Exp }
Exp : '[' ']' { ENone } 
  | '[' 'Let' CIdent Exp ']' { EAsign $3 $4 }
  | '[' 'Yield' Exp ']' { EYield $3 }
  | '[' 'True' ']' { ETrue }
  | '[' 'False' ']' { EFalse }
  | '[' 'For' CIdent Exp Exp ListStm ']' { EFor $3 $4 $5 (reverse $6) }
  | '[' 'While' Exp ListStm ']' { EWhile $3 (reverse $4) }
  | Integer { EInt $1 }
  | '-' Integer { ENegInt $2 }
  | String { EString $1 }
  | CIdent { EIdent $1 }
  | '[' Exp ListExp ']' { EFunPar $2 (reverse $3) }


ListExp :: { [Exp] }
ListExp : {- empty -} { [] } 
  | ListExp Exp { flip (:) $1 $2 }


FunParam :: { FunParam }
FunParam : '@' CIdent { FunParam $2 } 


ListFunParam :: { [FunParam] }
ListFunParam : {- empty -} { [] } 
  | ListFunParam FunParam { flip (:) $1 $2 }


FunDeclaration :: { FunDeclaration }
FunDeclaration : '[' 'Def' CIdent ListFunParam ListStm ']' { FunDec $3 (reverse $4) (reverse $5) } 


ListFunDeclaration :: { [FunDeclaration] }
ListFunDeclaration : {- empty -} { [] } 
  | ListFunDeclaration FunDeclaration { flip (:) $1 $2 }



{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}

