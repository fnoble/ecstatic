{-# LANGUAGE TypeOperators #-}
-- Potentially easier to understand than CExpression version
-- Not imported by any other module
module Development.Ecstatic.ExampleSimplify () where

import qualified Data.Map.Strict as M


type Name = String

data Term
  = L Int
  | V Name
  | Term :* Term
  | Term :+ Term
  deriving (Show, Eq, Ord)

-- Monomial
data T = T Name
  deriving (Show, Eq, Ord)

-- Product of monomials
data Prod = Prod [Name]
  deriving (Show, Eq, Ord)

type MAcc = (M.Map T Int, Int)
type AAcc = M.Map Prod Int

smul :: [MAcc] -> Term -> [MAcc]
smul accs (L number) = map (\(m, n) -> (m, n * number)) accs
smul accs (V name)   = map (\(m, n) -> (M.insertWith (+) (T name) 1 m, n)) accs
smul accs (t1 :* t2) = smul (smul accs t1) t2
smul accs (t1 :+ t2) = smul accs t1 ++ smul accs t2

toProd :: MAcc -> (Prod, Int)
toProd (m, n) = (Prod $ foldl f [] $ M.toList m, n)
 where
  f acc (T name, num) = replicate num name ++ acc

sadd :: AAcc -> Term -> AAcc
sadd acc (L number) = M.insertWith (+) (Prod []) number acc
sadd acc (t1 :+ t2) = sadd (sadd acc t1) t2
sadd acc t =
  let accs = smul [(M.empty, 1)] t
      ps   = map toProd accs
  in  
    foldl (\acc (prod, n) -> M.insertWith (+) prod n acc) acc ps

p1 = ((V "a") :* (V "b")) :* (V "c" :* V "a") :* (L 5) :* (L 2)
p2 = ((V "a") :* (V "a")) :* (V "a" :* V "b") :* (L 5) :* (L 2)
s1 = p1 :+ p2
s2 = (V "x" :+ V"y") :* (L 2)

simplify = sadd M.empty 
