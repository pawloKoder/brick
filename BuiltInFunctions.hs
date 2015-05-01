module BuiltInFunctions (builtInFunctions) where

import Control.Monad
import Control.Monad.Error

import RunUtils


builtInFunctions :: [(String, ExeFunction)]
builtInFunctions = [
    -- Logic
    ("And", bfAnd),
    ("Or", bfOr),
    ("Not", bfNot)
    ] ++ comparisonFunctions


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


