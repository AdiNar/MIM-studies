module Main where

import Control.Monad.Trans.Reader
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Identity
import qualified Data.Map as M
import System.IO
import System.Environment

import LexRose
import ParRose
import PrintRose
import AbsRose
import ErrM

import TypeCheck
import InterpreterRose

parse :: String -> Err Program
parse s = pProgram (myLexer s)

evaluate fun topExp =
  runIdentity $ runExceptT $ runReaderT (fun topExp) M.empty

printErr e = hPutStrLn stderr (show e) >> return ""
runRose :: String -> IO String
runRose s = do
    let Ok (Prog initialDecls) = parse initialCode 
    case parse s of
      Ok (Prog decls) ->
        -- let initialDecls in let [decls] in main
        let topExp = ELetin initialDecls $ ELetin decls $ EVType $ VIdent "main"
        in
          -- TypeCheck
          case evaluate check topExp of
            Left e -> printErr e
            -- Interpret
            _ -> case evaluate eval topExp of
                   Left e -> printErr e
                   Right x -> return $ show x
      Bad str -> return str

main = readAndRun >>= putStrLn

readAndRun = getArgs >>= \x -> case x of
  [filename] -> readFile filename >>= runRose
  _ -> getContents >>= runRose
