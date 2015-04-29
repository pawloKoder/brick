module Absbrick where

-- Haskell module generated by the BNF converter


newtype CIdent = CIdent String deriving (Eq,Ord,Show)
data Program =
   Progr [FunDeclaration]
  deriving (Eq,Ord,Show)

data Stm =
   SIf Exp [Exp]
 | SIfSkip Exp [Exp]
 | SIfElse Exp [Exp] [Exp]
 | SJump Jump_stm
 | SExp Exp
  deriving (Eq,Ord,Show)

data Jump_stm =
   SjumpReturn
 | SjumpReturnV Exp
 | SjumpBreak
 | SjumpBreakV Exp
 | SjumpContinue
 | SjumpContinueV Exp
  deriving (Eq,Ord,Show)

data Exp =
   ENone
 | EAsign CIdent Exp
 | EFunPar Exp [Exp]
 | EYield [Exp]
 | ETrue
 | EFalse
 | EFor CIdent Exp Exp [Exp]
 | EWhile Exp [Exp]
 | EFunDef FunDeclaration
 | EInt Integer
 | EString String
 | EIdent CIdent
  deriving (Eq,Ord,Show)

data FunDeclaration =
   FunDec CIdent [Stm]
  deriving (Eq,Ord,Show)

