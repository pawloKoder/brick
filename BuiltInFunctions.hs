module BuiltInFunctions (builtInFunctions) where

import Control.Monad
import Control.Monad.Error
import System.Exit
import Data.List

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
    ("Concat", bfConcat),
    -- List
    ("Append", bfAppend),
    ("List", bfList),
    -- Dict
    ("Dict", bfDict),
    ("Set", bfDictSet),
    ("Get", bfDictGet)
    ] ++ comparisonFunctions
    ++ aritmeticFunctions
    ++ listFunctions
    ++ dictFunctions


bfAnd :: ExeFunction
bfAnd l = liftM (BVBool . and) $ mapM boolCast l


bfOr :: ExeFunction
bfOr l = liftM (BVBool . or) $ mapM boolCast l


bfNot :: ExeFunction
bfNot [a] = liftM (BVBool . not) $ boolCast a
bfNot l = throwError $ "RTE: Not " ++ show l


genericOperator :: String -> (BValue -> BValue -> Bool) -> ExeFunction
genericOperator name operator [a, b] = do
    return $ BVBool (operator a b)
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
             (BVInt value) -> print value
             (BVString value) -> print value
             (BVBool value) -> print value
             (BVList value) -> print value
             (BVDict value) -> print value
             _ -> print var
    liftIO $ mapM_ printVar l
    return BVNone


bfConcat :: ExeFunction
bfConcat l = liftM (BVString . concat) $ mapM stringCast l


bfAppend :: ExeFunction
bfAppend [list, element] = liftM BVList $ liftM2 (++) (listCast list) (return [element])
bfAppend l = throwError $ "RTE: Append error" ++ show l


genericListFunction :: String -> ([BValue] -> BValue) -> ExeFunction
genericListFunction name operator [list] = liftM operator $ listCast list
genericListFunction name operator l = throwError $ "RTE: " ++ name ++ " " ++ show l

listFunctions = map (\(name, op) -> (name, genericListFunction name op))[
    ("Init", BVList . init),
    ("Last", last),
    ("Tail", BVList . tail),
    ("Head", head),
    ("Len", BVInt . fromIntegral . length)
    ]

bfList :: ExeFunction
bfList l = return $ BVList l


genericDictFunction :: String -> ([(BValue, BValue)] -> BValue) -> ExeFunction
genericDictFunction name operator [list] = liftM operator $ dictCast list
genericDictFunction name operator l = throwError $ "RTE: " ++ name ++ " " ++ show l

dictFunctions = map (\(name, op) -> (name, genericDictFunction name op))[
    ("Size", BVInt . fromIntegral . length),
    ("Keys", BVList . fst . unzip),
    ("Values", BVList . snd . unzip)
    ]


bfDict :: ExeFunction
bfDict [] = return $ BVDict []
bfdict l = throwError $ "RTE: Dict constructor error" ++ show l


bfDictSet :: ExeFunction
bfDictSet [dict, key, value] = do
    dictList <- liftM2 filter (return ((/= key).fst)) (dictCast dict)
    return $ BVDict $ (:) (key, value) dictList
bfDictSet l = throwError $ "RTE: Dict Set error" ++ show l


bfDictGet :: ExeFunction
bfDictGet [dict, key] = do
    res <- liftM2 lookup (return key) (dictCast dict)
    case res of
        Nothing -> throwError $ "RTE: Key error" ++ show key
        Just value -> return value
bfdictGet l = throwError $ "RTE: Dict Get error" ++ show l