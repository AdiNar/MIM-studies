{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

module InterpreterRose where

import Control.Monad.Trans.Reader
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Identity
import qualified Data.Map as M

import LexRose
import ParRose
import PrintRose
import AbsRose
import ErrM

data InterpExcept =
  UndefinedVariable String
  | AppToConstant
  | ArythmeticOpCalledOnNonInt Value Value
  | BoolOpOnNonBool Value Value
  | NotABool Value
  -- Unable to match pattern in pattern definition
  | IrrefutablePattern Pattern Value
  | NoMatchingPattern Value
  deriving Show

type EnvMap = M.Map String Value

data Value =
  IntVal Integer
  -- FunVal keeps environment (EnvMap) that comes from CallVal but also keeps already applied arguments.
  -- (String) is a name of next argument. (Exp) is a body of this function - that might be also another
  -- lambda that needs next argument.
  | FunVal EnvMap String Exp
  -- CallVall is a top-levep wrapper for FunVal or an algebraic type constructor.
  -- Keeps its body (Exp) and fixed point environment (EnvMap) that allows recursion and static visibility.
  | CallVal EnvMap Exp
  -- AlgVal is a 
  | AlgVal String [Value]
  deriving Eq

-- Because there are no predefined True and False values,
-- those are their equivalents as algebraic types.
pattern TrueVal = AlgVal "True" []
pattern FalseVal = AlgVal "False" []
-- List is also an algebraic type.
pattern ListVal head tail = AlgVal "List" [head, tail]
pattern EmptyListVal = AlgVal "Empty" []

instance Show Value where
  show (IntVal i) = show i
  show (AlgVal name args) = name ++ " " ++ show args
  show (FunVal typ name _) = name ++ " :: " ++ show typ
  show (CallVal _ _) = "Showing callable value"
  
type Parser = ReaderT EnvMap (ExceptT InterpExcept Identity)

eLookup :: String -> EnvMap -> Parser Value 
eLookup name env =
  case M.lookup name env of
    Nothing -> throwError (UndefinedVariable name)
    Just v -> return v
eLookupOrDefault :: Value -> String -> EnvMap -> Parser Value 
eLookupOrDefault def name env =
  case M.lookup name env of
    Nothing -> return def
    Just v -> return v

eval :: Exp -> Parser Value
eval (EAdd e1 e2) = do
  evalIntOp e1 e2 (+)
eval (ESub e1 e2) = do
  evalIntOp e1 e2 (-)
eval (EMul e1 e2) = do
  evalIntOp e1 e2 (*)
eval (EDiv e1 e2) = do
  evalIntOp e1 e2 div
eval (EAnd e1 e2) = do
  evalBoolOp e1 e2 (&&)
eval (EOr e1 e2) = do
  evalBoolOp e1 e2 (||)
eval (ELt e1 e2) = do
  evalIntToBoolOp e1 e2 (<)
eval (ELte e1 e2) = do
  evalIntToBoolOp e1 e2 (<=)
eval (EGt e1 e2) = do
  evalIntToBoolOp e1 e2 (>)
eval (EGte e1 e2) = do
  evalIntToBoolOp e1 e2 (>=)
eval (EEq e1 e2) = do
  evalValueToBoolOp e1 e2 (==)
eval (ENe e1 e2) = do
  evalValueToBoolOp e1 e2 (/=)
eval (EInt n) = return (IntVal n)
eval (EVType (VIdent x)) = ask >>= eLookup x
-- Type constructor is wrapped with CallVal, it should be extracted
eval (EType (TIdent x)) =
  ask >>= eLookup x >>= (\(CallVal env' exp) -> local (\_ -> env') (eval exp))
eval (EList []) = return EmptyListVal
eval (EList (x:xs)) = do
  head <- eval x
  tail <- eval (EList xs)
  return $ ListVal head tail
eval (ELetin [] exp) = eval exp
eval (ELetin (x:xs) exp) = do
  newEnv <- evalDeclWithReverse x
  local (\_ -> newEnv) (eval (ELetin xs exp))
eval (ECond cond e1 e2) = do
  val <- eval cond
  case val of 
    TrueVal -> eval e1
    FalseVal -> eval e2
    _ | otherwise -> throwError (NotABool val)
    
eval (ELambda _ (VIdent name) exp) = do
  env <- ask
  return $ FunVal env name exp
eval (ECase exp patts) = do
  env <- ask
  val <- eval exp
  let res =
        let f (PEntry patt pe) =
              case evalPattern patt val env of
                Just env -> Just (env, pe)
                _ -> Nothing
        in msum (map f patts)
  case res of
    Just (env, patExp) -> local (\_ -> env) (eval patExp)
    _ -> throwError $ NoMatchingPattern val
eval (EApp e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case v1 of
    FunVal env' ident body ->
      local (\_ -> M.insert ident v2 env') (eval body)
    CallVal env' body -> do
      FunVal env'' ident body' <- local (\_ -> env') (eval body)
      local (\_ -> M.insert ident v2 env'') (eval body')
    _ -> throwError AppToConstant
eval (EAlgType (TIdent name) 0) = return $ AlgVal name []
eval (EAlgType ident@(TIdent name) cnt) = do
  env <- ask
  IntVal current <- eLookupOrDefault (IntVal 0) "<cnt" env
  head <- eLookup (show current) env
  AlgVal name tail <- if (current+1) >= cnt then
    (return $ AlgVal name []) else
    local (M.insert "<cnt" $ IntVal (current+1)) (eval $ EAlgType ident cnt)
  return $ AlgVal name (head:tail)

evalIntOp :: Exp -> Exp -> (Integer -> Integer -> Integer) -> Parser Value
evalIntOp e1 e2 op = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (IntVal i1, IntVal i2) ->  return $ IntVal (op i1 i2)
    _ -> throwError $ ArythmeticOpCalledOnNonInt v1 v2

evalBoolOp :: Exp -> Exp -> (Bool -> Bool -> Bool) -> Parser Value
evalBoolOp e1 e2 op = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (AlgVal name1 [], AlgVal name2 []) -> makeBoolOp (toBool name1) (toBool name2) op
    _ -> throwError $ BoolOpOnNonBool v1 v2

evalIntToBoolOp :: Exp -> Exp -> (Integer -> Integer -> Bool) -> Parser Value
evalIntToBoolOp e1 e2 op = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (IntVal i1, IntVal i2) -> makeBoolOp i1 i2 op

evalValueToBoolOp :: Exp -> Exp -> (Value -> Value -> Bool) -> Parser Value
evalValueToBoolOp e1 e2 op = do
  v1 <- eval e1
  v2 <- eval e2
  makeBoolOp v1 v2 op

toBool "True" = True
toBool "False" = False

makeBoolOp :: a -> a -> (a -> a -> Bool) -> Parser Value
makeBoolOp v1 v2 op = do
  return $ if op v1 v2 then TrueVal else FalseVal

evalDeclWithReverse :: Decl -> Parser EnvMap
-- Function or type arguments are needed in reversed order
-- (due to the order of application).
evalDeclWithReverse (DFun typ ident args exp) =
  evalDecl (DFun typ ident (reverse args) exp)
evalDeclWithReverse (DType ident vars args) =
  evalDecl (DType ident vars (reverse args))
evalDeclWithReverse decl = evalDecl decl

evalDecl :: Decl -> Parser EnvMap
evalDecl (DFun _ (VIdent name) [] exp) = do
  env <- ask
  let env' = M.insert name (CallVal env' exp) env -- fixed point recursion
  return $ env'
evalDecl (DFun typ name (arg:args) exp) = do
  evalDecl (DFun typ name args newExp)
  where newExp = ELambda typ arg exp
-- That looks a bit tricky: Type definition is turned to a function that,
-- given type arguments, returns variable of this type. Arguments are named
-- with numbers - that is totally safe because grammar does not allow numeric
-- variables so there's nothing under those fields in environment.
evalDecl (DType typ _ args) = do
  env <- ask
  return $ foldr f env args
  where
    f (TAlg (TIdent name) arg) env' =
          M.insert name (CallVal M.empty $ fexp name arg 0) env'
    fexp name [] cnt = EAlgType (TIdent name) cnt
    fexp name (x:xs) cnt = ELambda
      (TAlg (TIdent "") []) -- any type, it's irrelevant at this moment.
      (VIdent $ show cnt)
      (fexp name xs (cnt+1))
-- Because there's pattern declaration mechanism (let (x:xs) = [1,2]) in language
-- even simple declarations like let a = 1 are treated as patterns.
evalDecl (DPattern patt exp) = do
  env <- ask
  val <- eval exp
  case evalPattern patt val env of
    Just env -> return env
    _ -> throwError $ IrrefutablePattern patt val

-- Pattern evaluation: Just env on successfull matching, Nothing otherwise
evalPattern :: Pattern -> Value -> EnvMap -> Maybe EnvMap
-- Matching to a variable, always successful
evalPattern (PVPattern (VIdent name)) val env =
  Just $ M.insert name val env
-- Underscore, matches everything
evalPattern PAnyPattern _ env = Just env
-- (x:xs) pattern
evalPattern (PListPattern head tail) val env =
  case val of
    ListVal x xs ->
      evalPattern head x env >>= evalPattern tail xs
    _ -> Nothing
-- Numeric pattern, just checking for value
evalPattern (PInt int) val env =
  case val of
    IntVal ival | int == ival -> Just env
    _ -> Nothing
-- [] Pattern
evalPattern (PList []) val env =
  case val of
    EmptyListVal -> Just env
    _ -> Nothing
-- [x,y,z] pattern
evalPattern (PList (x:xs)) val env =
  case val of
    ListVal v vs ->
      evalPattern x v env >>= evalPattern (PList xs) vs
    _ -> Nothing
-- Algebraic type matching
evalPattern (PAlgPattern ident@(TIdent name) patts) val env =
  case (val, patts) of
    (AlgVal vName [], []) | name == vName -> Just env
    (AlgVal vName (a:as), (p:ps))
      | name == vName -> evalPattern p a env >>= evalPattern (PAlgPattern ident ps) (AlgVal vName as) 
    _ -> Nothing

initialCode = "data Bool () = True () | False (); \
              \data Int () = Int (); \
              \data List () = List (<a> (List (<a>))) | Empty ()"
