{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Development.Ecstatic.Utils
import Development.Ecstatic.StackUsage

import Language.C
import Language.C.Analysis
import Text.Printf
import System.Console.ANSI
import qualified Text.PrettyPrint as PP
import System.Environment (getArgs)


declHeader :: VarDecl -> String
declHeader d =
  printf "%s (%s:%d):" name file line
  where
    i = declIdent d
    name = identToString i
    p = posOfNode . nodeInfo $ i
    line = posRow p
    file = posFile p

analyseFunc :: GlobalDecls -> FunDef -> IO ()
analyseFunc g (FunDef d s _) = do
  -- Print function name header
  setSGR [SetColor Foreground Dull Red]
  putStrLn $ declHeader d
  setSGR [Reset]

  _ <- printCallGraph g s
  return ()

-- PP the parsed AST
reprint :: IO String
reprint = do
  ast <- parseFile "test2.c"
  return $ PP.render . prettyUsingInclude $ ast

analyzeFile :: FilePath -> IO ()
analyzeFile file = do
  mast <- parseASTFile file
  case mast of
    Nothing -> putStrLn "analyzeFile: Invalid file."
    Just (globals, funcs) ->
      mapM_ (analyseFunc globals) $ funcs

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> return ()
    file : _ -> analyzeFile file
      

