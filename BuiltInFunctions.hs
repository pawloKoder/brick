module BuiltInFunctions (builtInFunctions) where

import Control.Monad
import Control.Monad.Error
import System.Exit

import RunUtils


builtInFunctions :: [(String, ExeFunction)]
builtInFunctions = [
    -- Logic
    ("And", bfAnd),
    ("Or", bfOr),
    ("Not", bfNot),
    -- Misc
    ("Exit", bfExit),
    -- IO
    ("PrintLn", bfPrintLn),
    -- String
    ("Concat", bfConcat)
    ] ++ comparisonFunctions
    ++ aritmeticFunctions


bfAnd :: ExeFunction
bfAnd l = liftM (BVBool . and) $ mapM boolCast l


bfOr :: ExeFunction
bfOr l = liftM (BVBool . or) $ mapM boolCast l


bfNot :: ExeFunction
bfNot [a] = liftM (BVBool . not) $ boolCast a
bfNot l = throwError $ "RTE: Not " ++ show l


genericOperator :: String -> (BValue -> BValue -> Bool) -> ExeFunction
genericOperator name operator [a, b] = return $ BVBool (operator a b)
genericOperator name operator l = throwError $ "RTE: " ++ name ++ " " ++ show l


comparisonFunctions = map (\(name, op) -> (name, genericOperator name op))[
    ("Gt", (>)),
    ("Ge", (>=)),
    ("Lt", (<)),
    ("Le", (<=)),
    ("Eq", (==)),
    ("CastEq", (==)),
    ("Neq", (/=))
    ]


genericIntAritmetic :: String -> (Integer -> Integer -> Integer) -> ExeFunction
genericIntAritmetic name transform [a, b] = liftM BVInt $ liftM2 transform (integerCast a) (integerCast b)
genericIntAritmetic name transform l = throwError $ "RTE: " ++ name ++ " " ++ show l
aritmeticFunctions = map (\(name, op) -> (name, genericIntAritmetic name op))[
    ("Add", (+)),
    ("Sub", (-)),
    ("Mul", (*))
    ]


bfExit :: ExeFunction
bfExit [(BVInt result)] = do
    if result == 0
        then liftIO exitSuccess
        else throwError $ "Program ended with error code: " ++ show result
    return BVNone


bfPrintLn :: ExeFunction
bfPrintLn l = do
    let printVar var = do
        case var of
             _ -> print var
    liftIO $ mapM_ printVar l
    return BVNone


bfConcat :: ExeFunction
bfConcat l = liftM (BVString . concat) $ mapM stringCast l

