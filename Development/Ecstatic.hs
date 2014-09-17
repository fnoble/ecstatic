{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Development.Ecstatic.Utils
import Development.Ecstatic.BoundsCheck
import Development.Ecstatic.StackUsage

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


includes = [
  "-I../../swiftnav/piksi_firmware/libswiftnav/include/libswiftnav",
  "-I../../swiftnav/piksi_firmware/libswiftnav/src",
  "-I../../swiftnav/piksi_firmware/libswiftnav/clapack-3.2.1-CMAKE/INCLUDE",
  "-I../../swiftnav/piksi_firmware/libswiftnav/CBLAS/include" ]

parseFile :: FilePath -> IO CTranslUnit
parseFile input_file =
  do parse_result <- parseCFile (newGCC "gcc") Nothing includes input_file
     checkResult "[Parsing]" parse_result

checkResult :: (Show a) => String -> (Either a b) -> IO b
checkResult label = either (error . (label++) . show) return

extractFuncs :: DeclEvent -> Trav [FunDef] ()
extractFuncs (DeclEvent (FunctionDef f)) = do
  modifyUserState (\x -> f:x)
  return ()
extractFuncs _ = return ()

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
  doStackUsage g s

-- PP the parsed AST
reprint :: IO String
reprint = do
  ast <- parseFile "test2.c"
  return $ PP.render . prettyUsingInclude $ ast

main :: IO ()
main = do
  -- ast <- parseFile "test.c"
  ast <- parseFile "test2.c"
  --ast <- parseFile "../../SwiftNav/piksi_firmware/libswiftnav/src/amb_kf.c"
  (globals, funcs) <- checkResult "[Analysis]" . runTrav [] $
    withExtDeclHandler (analyseAST ast) extractFuncs
  --print "GLOBALS:\n"
  --print $ pretty globals
  mapM_ (analyseFunc globals) $ userState funcs

