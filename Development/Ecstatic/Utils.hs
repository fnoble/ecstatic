{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Development.Ecstatic.Utils (
  substitute, subByName, simplify,
  sortWith,
  parseFile, checkResult,
  pp
) where

import Language.C
import Language.C.Data.Ident
import Language.C.System.GCC
import Data.Generics.Uniplate.Data
import Data.Typeable
import Data.Data
import Data.List
import Control.Monad
import Debug.Trace

includes = [
  "-nostdinc",
  "-I./mocks/",

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


pp :: CExpr -> String
pp = show . pretty

checkResult :: (Show a) => String -> (Either a b) -> IO b
checkResult label = either (error . (label++) . show) return

parseFile :: FilePath -> IO CTranslUnit
parseFile input_file =
  do parse_result <- parseCFile (newGCC "gcc") Nothing includes input_file
     checkResult "[Parsing]" parse_result

sortWith f = sortBy (\x y -> compare (f x) (f y))

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
        f v@(CVar i@(Ident s _ _) _) = if n == s then e else v
        f x = x

dummyNodeInfo :: NodeInfo
dummyNodeInfo = OnlyPos nopos (nopos, 0)

instance Num (CExpression NodeInfo) where
  e1 + e2 = CBinary CAddOp e1 e2 dummyNodeInfo
  e1 - e2 = CBinary CSubOp e1 e2 dummyNodeInfo
  e1 * e2 = CBinary CMulOp e1 e2 dummyNodeInfo
  negate e = CUnary CMinOp e dummyNodeInfo
  fromInteger x = CConst (CIntConst (CInteger x DecRepr noFlags) dummyNodeInfo)
  -- abs
  -- signum

simplify :: CExpression NodeInfo -> CExpression NodeInfo
simplify = rewrite $ \e ->
    simpAddZero e `mplus`
    simpMulOne e `mplus`
    simpConst e

simpAddZero :: CExpression NodeInfo -> Maybe (CExpression NodeInfo)
simpAddZero (CBinary CAddOp e (CConst (CIntConst (CInteger 0 _ _) _)) _) = Just e
simpAddZero (CBinary CAddOp (CConst (CIntConst (CInteger 0 _ _) _)) e _) = Just e
simpAddZero (CBinary CSubOp e (CConst (CIntConst (CInteger 0 _ _) _)) _) = Just e
simpAddZero (CBinary CSubOp (CConst (CIntConst (CInteger 0 _ _) _)) e _) = Just e
simpAddZero _ = Nothing

simpMulOne :: CExpression NodeInfo -> Maybe (CExpression NodeInfo)
simpMulOne (CBinary CMulOp e (CConst (CIntConst (CInteger 1 _ _) _)) _) = Just e
simpMulOne (CBinary CMulOp (CConst (CIntConst (CInteger 1 _ _) _)) e _) = Just e
simpMulOne _ = Nothing

simpConst :: CExpression NodeInfo -> Maybe (CExpression NodeInfo)
-- (x + y) => x+y
simpConst (CBinary CAddOp
  (CConst (CIntConst (CInteger x c1 c2) c3))
  (CConst (CIntConst (CInteger y _ _) _)) _) =
    Just (CConst (CIntConst (CInteger (x + y) c1 c2) c3))
-- ((x + ?) + y) => (? + (x+y)
simpConst (CBinary CAddOp
    (CBinary CAddOp
      (CConst (CIntConst (CInteger x c1 c2) c3))
      e _)
    (CConst (CIntConst (CInteger y _ _) _)) c4) =
    Just (CBinary CAddOp e (CConst (CIntConst (CInteger (x + y) c1 c2) c3)) c4)
-- ((? + x) + y) => (? + (x+y)
simpConst (CBinary CAddOp
    (CBinary CAddOp
      e
      (CConst (CIntConst (CInteger x c1 c2) c3)) _)
    (CConst (CIntConst (CInteger y _ _) _)) c4) =
    Just (CBinary CAddOp e (CConst (CIntConst (CInteger (x + y) c1 c2) c3)) c4)
-- (y + (x + ?)) => (? + (x+y)
simpConst (CBinary CAddOp
    (CConst (CIntConst (CInteger y _ _) _))
    (CBinary CAddOp
      (CConst (CIntConst (CInteger x c1 c2) c3))
      e _) c4) =
    Just (CBinary CAddOp e (CConst (CIntConst (CInteger (x + y) c1 c2) c3)) c4)
-- (y + (? + x)) => (? + (x+y)
simpConst (CBinary CAddOp
    (CConst (CIntConst (CInteger y _ _) _))
    (CBinary CAddOp
      e
      (CConst (CIntConst (CInteger x c1 c2) c3)) _) c4) =
    Just (CBinary CAddOp e (CConst (CIntConst (CInteger (x + y) c1 c2) c3)) c4)

simpConst (CBinary CSubOp
  (CConst (CIntConst (CInteger x c1 c2) c3))
  (CConst (CIntConst (CInteger y _ _) _)) _) =
    Just (CConst (CIntConst (CInteger (x - y) c1 c2) c3))

simpConst (CBinary CMulOp
  (CConst (CIntConst (CInteger x c1 c2) c3))
  (CConst (CIntConst (CInteger y _ _) _)) _) =
    Just (CConst (CIntConst (CInteger (x * y) c1 c2) c3))

simpConst _ = Nothing


