{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Development.Ecstatic.Utils (
  substitute, subByName, --simplify,
  mapFst, mapSnd, sortWith,
  parseFile, checkResult, parseAST, parseASTFile
) where

import Language.C
import Language.C.Data.Ident
import Language.C.System.GCC
import Language.C.Analysis
import Data.Generics.Uniplate.Data
import Data.Typeable
import Data.Data
import Data.List

includes :: [FilePath]
includes = [
  "-nostdinc",
 -- "-I./mocks/",

  "-I../../swift/piksi_firmware/libswiftnav/include/libswiftnav",
  "-I../../swift/piksi_firmware/libswiftnav/include/",
  "-I../../swift/piksi_firmware/libopencm3/include/",

  "-I../../swift/piksi_firmware/ChibiOS-RT/os/kernel/include/",
  "-I../../swift/piksi_firmware/ChibiOS-RT/os/ports/GCC/ARMCMx/",
  "-I../../swift/piksi_firmware/ChibiOS-RT/os/ports/GCC/ARMCMx/STM32F4xx/",
  "-I../../swift/piksi_firmware/ChibiOS-RT/os/ports/common/ARMCMx/",

  "-I../../swift/piksi_firmware/libswiftnav/src",
  "-I../../swift/piksi_firmware/libswiftnav/clapack-3.2.1-CMAKE/INCLUDE",
  "-I../../swift/piksi_firmware/libswiftnav/CBLAS/include",
  "-I../../swift/piksi_firmware/src"
  ]


-- General Stuff
mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)
mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (a, b) = (a, f b)

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith f = sortBy (\x y -> compare (f x) (f y))

-- Substitution functions

-- Substitute a variable for an expression throughout anything that contains
-- expressions (e.g. AST, CExpression, CStatement)
substitute :: forall a b . (Data a, Typeable a, Data b, Typeable b) =>
                Ident -> CExpression b -> a -> a
substitute i e = transformBi f
  where f :: CExpression b -> CExpression b
        f v@(CVar i' _) = if i == i' then e else v
        f x = x

-- Substitute any variable matching a given name for an expression throughout
-- anything that contains expressions (e.g. AST, CExpression, CStatement)
subByName :: forall a b . (Data a, Typeable a, Data b, Typeable b) =>
                String -> CExpression b -> a -> a
subByName n e = transformBi f
  where f :: CExpression b -> CExpression b
        f v@(CVar (Ident s _ _) _) = if n == s then e else v
        f x = x

-- Used by StackUsage
dummyNodeInfo :: NodeInfo
dummyNodeInfo = OnlyPos nopos (nopos, 0)
instance Num (CExpression NodeInfo) where
  e1 + e2 = CBinary CAddOp e1 e2 dummyNodeInfo
  e1 - e2 = CBinary CSubOp e1 e2 dummyNodeInfo
  e1 * e2 = CBinary CMulOp e1 e2 dummyNodeInfo
  negate e = CUnary CMinOp e dummyNodeInfo
  fromInteger x = CConst (CIntConst (CInteger x DecRepr noFlags) dummyNodeInfo)
  abs = undefined
  signum = undefined
  -- abs
  -- signum

-- Parsing Stuff --
checkResult :: (Show a) => String -> (Either a b) -> IO b
checkResult label = either (error . (label++) . show) return

parseFile :: FilePath -> IO CTranslUnit
parseFile input_file =
  do parse_result <- parseCFile (newGCC "gcc") Nothing includes input_file
     checkResult "[Parsing]" parse_result

extractFuncs :: DeclEvent -> Trav [FunDef] ()
extractFuncs (DeclEvent (FunctionDef f)) = do
  modifyUserState (\x -> f:x)
  return ()
extractFuncs _ = return ()

parseAST :: CTranslUnit -> Either [CError] (GlobalDecls, [FunDef])
parseAST ast = do
  (globals, funcs) <- runTrav [] $ withExtDeclHandler (analyseAST ast) extractFuncs
  return $ (globals, userState funcs)

parseASTFile :: FilePath -> IO (Maybe (GlobalDecls, [FunDef]))
parseASTFile file = do
  ast <- parseFile file
  case parseAST ast of
    Left err -> do putStrLn $ "bad ast: " ++ show err
                   return Nothing
    Right (globals, fns) -> return (Just (globals, fns))

-- TODO only used for error reporting/debugging; remove?
deriving instance Show EnumTypeRef
deriving instance Show CompTypeRef
deriving instance Show VarName
deriving instance Show BuiltinType
deriving instance Show DeclAttrs
deriving instance Show TypeQuals
deriving instance Show TypeName
deriving instance Show VarDecl
deriving instance Show ParamDecl
deriving instance Show TypeDefRef
deriving instance Show FunType
deriving instance Show ArraySize
deriving instance Show Type
deriving instance Show Attr
deriving instance Show TypeDef

deriving instance Show ObjDef
deriving instance Show EnumType
deriving instance Show FunDef
deriving instance Show Enumerator
deriving instance Show Decl
deriving instance Show IdentDecl

deriving instance Show MemberDecl
deriving instance Show CompType
deriving instance Show TagDef

