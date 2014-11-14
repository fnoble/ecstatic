{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
module Development.Ecstatic.Types where
import Language.C
import Data.Typeable
import Data.Data

instance Num (CExpression NodeInfo) where
  e1 + e2 = CBinary CAddOp e1 e2 undefNode
  e1 - e2 = CBinary CSubOp e1 e2 undefNode
  e1 * e2 = CBinary CMulOp e1 e2 undefNode
  negate e = CUnary CMinOp e undefNode
  fromInteger x = CConst (CIntConst (CInteger x DecRepr noFlags) undefNode)

  abs = undefined
  signum = undefined

