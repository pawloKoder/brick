{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Printbrick where

-- pretty-printer generated by the BNF converter

import Absbrick
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: [a] -> Doc
  prtList = concatD . map (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)



instance Print CIdent where
  prt _ (CIdent i) = doc (showString ( i))



instance Print Program where
  prt i e = case e of
   Progr fundeclarations -> prPrec i 0 (concatD [prt 0 fundeclarations])


instance Print Stm where
  prt i e = case e of
   SIf exp stms -> prPrec i 0 (concatD [doc (showString "[") , doc (showString "If") , prt 0 exp , prt 0 stms , doc (showString "]")])
   SIfElse exp stms0 stms -> prPrec i 0 (concatD [doc (showString "[") , doc (showString "If") , prt 0 exp , prt 0 stms0 , doc (showString "]") , doc (showString "[") , doc (showString "Else") , prt 0 stms , doc (showString "]")])
   SJump jumpstm -> prPrec i 0 (concatD [prt 0 jumpstm])
   SExp exp -> prPrec i 0 (concatD [prt 0 exp])
   SFunDef fundeclaration -> prPrec i 0 (concatD [prt 0 fundeclaration])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print JumpStm where
  prt i e = case e of
   SjumpReturn  -> prPrec i 0 (concatD [doc (showString "[") , doc (showString "Return") , doc (showString "]")])
   SjumpReturnV exp -> prPrec i 0 (concatD [doc (showString "[") , doc (showString "Return") , prt 0 exp , doc (showString "]")])
   SjumpBreak  -> prPrec i 0 (concatD [doc (showString "[") , doc (showString "Break") , doc (showString "]")])
   SjumpBreakV exp -> prPrec i 0 (concatD [doc (showString "[") , doc (showString "Break") , prt 0 exp , doc (showString "]")])
   SjumpContinue  -> prPrec i 0 (concatD [doc (showString "[") , doc (showString "Continue") , doc (showString "]")])
   SjumpContinueV exp -> prPrec i 0 (concatD [doc (showString "[") , doc (showString "Continue") , prt 0 exp , doc (showString "]")])


instance Print Exp where
  prt i e = case e of
   ENone  -> prPrec i 0 (concatD [doc (showString "[") , doc (showString "]")])
   EAsign cident exp -> prPrec i 0 (concatD [doc (showString "[") , doc (showString "Let") , prt 0 cident , prt 0 exp , doc (showString "]")])
   EYield exp -> prPrec i 0 (concatD [doc (showString "[") , doc (showString "Yield") , prt 0 exp , doc (showString "]")])
   ETrue  -> prPrec i 0 (concatD [doc (showString "[") , doc (showString "True") , doc (showString "]")])
   EFalse  -> prPrec i 0 (concatD [doc (showString "[") , doc (showString "False") , doc (showString "]")])
   EFor cident exp0 exp stms -> prPrec i 0 (concatD [doc (showString "[") , doc (showString "For") , prt 0 cident , prt 0 exp0 , prt 0 exp , prt 0 stms , doc (showString "]")])
   EWhile exp stms -> prPrec i 0 (concatD [doc (showString "[") , doc (showString "While") , prt 0 exp , prt 0 stms , doc (showString "]")])
   EInt n -> prPrec i 0 (concatD [prt 0 n])
   ENegInt n -> prPrec i 0 (concatD [doc (showString "-") , prt 0 n])
   EString str -> prPrec i 0 (concatD [prt 0 str])
   EIdent cident -> prPrec i 0 (concatD [prt 0 cident])
   EFunPar exp exps -> prPrec i 0 (concatD [doc (showString "[") , prt 0 exp , prt 0 exps , doc (showString "]")])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print FunParam where
  prt i e = case e of
   FunParam cident -> prPrec i 0 (concatD [doc (showString "@") , prt 0 cident])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print FunDeclaration where
  prt i e = case e of
   FunDec cident funparams stms -> prPrec i 0 (concatD [doc (showString "[") , doc (showString "Def") , prt 0 cident , prt 0 funparams , prt 0 stms , doc (showString "]")])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])


