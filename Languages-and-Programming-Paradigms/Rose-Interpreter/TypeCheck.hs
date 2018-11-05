{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module TypeCheck where

import Control.Monad.Trans.Reader
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Identity
import qualified Data.Map as M
  
import AbsRose

data TypeCheckExcept =
  UndefinedType String
  | UndefinedVar String
  | TypeMismatch Value Value String
  | NotExpectedTypes Value Value Value
  | NotEqualTypes Value Value
  | TypeAlreadyDefined String
  | ExpectedCallable Value
  | PatternListMismatch Value
  | AppToConstantType
  | PatternError Value String

data Value =
  -- FunType <Argument type> <Type returned after type application>
  FunType Value Value
  | VarType String
  | AlgType String [Value]
  | FilledAlgType String [Value]
  deriving (Show, Eq)

instance Show TypeCheckExcept where
  show (UndefinedType str) =
    "Undefined type: " ++ str
  show (UndefinedVar str) =
    "Undefined variable: " ++ str
  show (TypeMismatch exp got str) =
    "Expected " ++ show exp ++ " got " ++ show got ++ " " ++ str
  show (NotExpectedTypes exp t1 t2) =
    "Expected " ++ show exp ++ ", but got: " ++ show t1 ++ " and " ++ show t2
  show (NotEqualTypes t1 t2) =
    "Types " ++ show t1 ++ " and " ++ show t2 ++ " should be equal"
  show (TypeAlreadyDefined str) = "Type " ++ str ++ " is already defined"
  show (ExpectedCallable got) = "Expected callable, got " ++ show got
  show (PatternListMismatch got) = "Pattern (_:_) expects list type, got: " ++ show got
  show AppToConstantType = "App to constant type"
  show (PatternError exp got) = "Pattern of type " ++ show exp ++ " expected, got " ++ got

type TypeMap = M.Map String Value
type Parser = ReaderT TypeMap (ExceptT TypeCheckExcept Identity)

pattern IntType = AlgType "Int" []
pattern BoolType = AlgType "Bool" []
pattern ListType a = AlgType "List" [a]
pattern EmptyListType = AlgType "Empty" []

eLookup :: String -> TypeMap -> Parser Value
eLookup name env =
  case M.lookup name env of
    Nothing -> throwError (UndefinedType name)
    Just v -> return v

check :: Exp -> Parser Value
check (EAdd e1 e2) = checkIntTypes  e1 e2
check (ESub e1 e2) = checkIntTypes  e1 e2
check (EMul e1 e2) = checkIntTypes  e1 e2
check (EDiv e1 e2) = checkIntTypes  e1 e2
check (EOr  e1 e2) = checkBoolTypes e1 e2
check (EAnd e1 e2) = checkBoolTypes e1 e2
check (EEq  e1 e2) = check e1 >> check e2 >> return BoolType
check (ENe  e1 e2) = check e1 >> check e2 >> return BoolType
check (ELt  e1 e2) = checkCompTypes e1 e2
check (ELte e1 e2) = checkCompTypes e1 e2
check (EGt  e1 e2) = checkCompTypes e1 e2
check (EGte e1 e2) = checkCompTypes e1 e2
check (EInt n) = return IntType
check (ECond cond e1 e2) = do
  condType <- check cond
  if condType == BoolType then do
    t1 <- check e1
    t2 <- check e2
    if t1 == t2 then return t1 else throwError $ NotEqualTypes t1 t2
  else throwError $ TypeMismatch BoolType condType " in condition"

check (ELetin [] exp) = check exp
check (ELetin (x:xs) exp) = do
  env <- checkDecl x
  local (\_ -> env) (check (ELetin xs exp))
check (EType (TIdent name)) = do
  env <- ask
  case M.lookup name env of
    Just x -> return x
    _ -> throwError $ UndefinedType name
check (EApp e1 e2) = do
  t1 <- check e1
  t2 <- check e2
  case t1 of
    FunType t t' -> 
      if t == t2 then return t'
      else throwError $ TypeMismatch t t2
           ("while applicating " ++ show e2 ++ " to " ++ show e1)
    t -> throwError $ ExpectedCallable t 

check (EVType (VIdent name)) = do
  env <- ask
  case M.lookup name env of
    Just x -> return x
    Nothing -> throwError $ UndefinedVar name

check (EList []) = do
  return $ EmptyListType
check (EList exps@(x:xs)) = do
  env <- ask
  t <- check x
  let f _ exp = check exp >>= requireType t 
  foldM f t exps
  return $ ListType t
check (ELambda typ (VIdent name) exp) = do
  env <- ask
  let parsedType = parseType typ
  case parsedType of
    FunType argType resDeclaredType -> do
      let env' = M.insert name argType env
      resType <- local (\_ -> env') (check exp)
      if resType == resDeclaredType
        then return $ FunType argType resType
        else throwError $ TypeMismatch resDeclaredType resType "in lambda body."
    _ -> throwError $ ExpectedCallable parsedType

check (ECase exp entries@((PEntry pat pExp):_)) = do
  env <- ask
  t <- check exp
  let f typ (PEntry pat exp) = do
        env' <- checkPattern pat t
        local (\_ -> env') (check exp) >>= requireType typ
  env' <- checkPattern pat t
  firstType <- local (\_ -> env') (check pExp)
  foldM f firstType entries
  
checkIntTypes  = checkTypes IntType  IntType
checkBoolTypes = checkTypes BoolType BoolType
checkCompTypes = checkTypes IntType  BoolType

-- Helper function for arythmetic operations
checkTypes :: Value -> Value -> Exp -> Exp -> Parser Value
checkTypes inT outT e1 e2 = do
  t1 <- check e1
  t2 <- check e2
  case (t1, t2) of
    (x, y) | x == inT && y == inT -> return outT
    _ -> throwError (NotExpectedTypes inT t1 t2)

checkDecl :: Decl -> Parser TypeMap
checkDecl (DPattern pat exp) = do
  t1 <- check exp
  checkPattern pat t1
 
checkDecl (DType (TIdent name) vTypes types) = do
  env <- ask
  let f env (TAlg (TIdent name') types') =
        case M.lookup name' env of
          Just _ -> throwError $ TypeAlreadyDefined name'
          _ -> return $ M.insert name' (algTypeToFun types' name) env 
  foldM f env types

checkDecl (DFun typ ident@(VIdent name) args exp) = do
  let parsed = parseType typ
  let resultType = extractLast parsed args
  env <- addTypes parsed args
  bodyType <- local (\_ -> M.insert name parsed env) (check exp)
  if resultType == bodyType then
    return $ M.insert name parsed env
  else throwError $ TypeMismatch resultType bodyType ("in body of function " ++ name)

-- Check types inside pattern and puts declarations from those patterns
checkPattern :: Pattern -> Value -> Parser TypeMap
checkPattern (PInt n) typ =
  requireType typ IntType >> ask
checkPattern PAnyPattern typ = ask
checkPattern (PListPattern p1 p2) typ = do
  case typ of
    ListType a -> do 
      env <- checkPattern p1 a
      local (\_ -> env) (checkPattern p2 typ)
    _ -> throwError $ PatternListMismatch typ
checkPattern (PList patts) typ = do
  env <- ask
  let f typ env pat =
        local (\_ -> env) (checkPattern pat typ)
  case typ of
    ListType a -> foldM (f a) env patts
    _ -> throwError $ PatternListMismatch typ
checkPattern (PVPattern (VIdent name)) typ = do
  env <- ask
  return $ M.insert name typ env
checkPattern (PAlgPattern (TIdent name) patts) typ = do
  env <- ask
  let f types env (pat,typ) = local (\_ -> env) (checkPattern pat typ) 
  case typ of
    AlgType x types -> do
      values <- funToAlg name env
      foldM (f types) env (zip patts values)
    _ -> throwError $ PatternError typ name

-- Turns type constructor to list of arguments types
funToAlg :: String -> TypeMap -> Parser [Value]
funToAlg name env = do
  case M.lookup name env of
    Just (FunType t1 t2) -> 
      let
        f (FunType t1 t2) = t1:(f t2)
        f x = [x]
      in return $ t1:(f t2)
    _ -> throwError $ UndefinedType name

-- require equality of arguments, throw error otherwise
requireType :: Value -> Value -> Parser Value
requireType t1 t2 =
  if t1 == t2 then return t1 else throwError $ TypeMismatch t1 t2 ""

-- Turns algebraic type definition to it's constructor function
algTypeToFun :: [Type] -> String -> Value
algTypeToFun types name =
  foldr f (AlgType name []) (map parseType types)
  where f typ act = FunType typ act

-- Extracts type of constant returned by last application
extractLast :: Value -> [a] -> Value
extractLast (FunType t1 t2) (x:xs) = extractLast t2 xs
extractLast t _ = t

-- Inserts types of function arguments to type map
addTypes :: Value -> [VIdent] -> Parser TypeMap
addTypes (FunType t _) [VIdent x] = do
  env <- ask
  return $ M.insert x t env
addTypes (FunType t1 t2) ((VIdent x):xs) = do
  local (M.insert x t1) (addTypes t2 xs)
addTypes typ [VIdent x] = do
  env <- ask
  return $ M.insert x typ env
addTypes typ x = ask

-- Turns parser type to a TypeChecker supported type
parseType :: Type -> Value
parseType (TCombined t1 t2) = FunType (parseType t1) (parseType t2)
parseType (TList typ) = ListType (parseType typ)
parseType (TVar (VIdent name)) = VarType name
parseType (TAlg (TIdent name) types) = AlgType name (map parseType types)
