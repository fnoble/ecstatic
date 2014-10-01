{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Development.Ecstatic.Utils
import Development.Ecstatic.BoundsCheck
import Development.Ecstatic.StackUsage
import Development.Ecstatic.Annotate

import qualified Development.Ecstatic.Simplify as S

import Language.C
import Language.C.Pretty
import Language.C.System.GCC
import Language.C.Analysis
import qualified Data.Map as M
import Debug.Trace
import Language.C.Data.Ident
import Text.Printf
import Control.Monad
import Data.List
import Control.Exception
import Data.Generics.Uniplate.Data
import System.Console.ANSI
import Data.Typeable
import Data.Data
import qualified Text.PrettyPrint as PP
import System.Environment (getArgs)


declHeader :: VarDecl -> String
declHeader d =
  printf "%s (%s:%d):" name file line
  where
    i = declIdent d
    name = identToString i
    p = case i of
      (Ident _ _ (NodeInfo pos _ _)) -> pos
    line = posRow p
    file = posFile p

analyseFunc :: GlobalDecls -> FunDef -> IO ()
analyseFunc g f@(FunDef d s i) = do
  -- Print function name header
  setSGR [SetColor Foreground Dull Red]
  putStrLn $ declHeader d
  setSGR [Reset]
  --print $ pretty d

  --boundsCheck s
  _ <- doStackUsage' g s
  return ()

-- PP the parsed AST
reprint :: IO String
reprint = do
  ast <- parseFile "test2.c"
  return $ PP.render . prettyUsingInclude $ ast

testRepl name = do
  ast <- parseFile "test.c"
  case parseAST ast of
    Left err -> putStrLn $ "bad ast: " ++ show err
    Right (globals, fns) -> 
      case findDef fns name of
        Nothing -> print "no def"
        Just def -> analyseFunc globals def

analyzeFile :: FilePath -> IO ()
analyzeFile file = do
  -- ast <- parseFile "test.c"
  ast <- parseFile file
  --ast <- parseFile "../../SwiftNav/piksi_firmware/libswiftnav/src/amb_kf.c"
  let Right (globals, funcs) = parseAST ast
  --print $ pretty globals
  mapM_ (analyseFunc globals) $ funcs

testMain = analyzeFile "test2.c"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> return ()
    file : _ -> analyzeFile file
      

