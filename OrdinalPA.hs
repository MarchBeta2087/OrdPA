{-# LANGUAGE BangPatterns #-}
module OrdinalPA where

import Prelude hiding (Ord)
import Data.List (intercalate)
import Numeric.Natural (Natural)

-- | ε0 以下的序数，Cantor 标准型：
--   α = ω^a1·c1 + ... + ω^ak·ck,  a1 > ... > ak,  ci > 0
-- 不变量：tCoef > 0，指数严格递减。
newtype Ord = Ord [Term]

data Term = Term
  { tExp  :: Ord      -- 这一项是 ω^tExp
  , tCoef :: Natural
  }

instance Eq Ord where
  Ord xs == Ord ys = xs == ys

instance Eq Term where
  Term a m == Term b n = a == b && m == n

-- 构造与规范
zero :: Ord
zero = Ord []

one :: Ord
one = finite 1

finite :: Natural -> Ord
finite 0 = zero
finite n = Ord [Term zero n]

omega :: Ord
omega = omegaPow one

omegaPow :: Ord -> Ord
omegaPow a = Ord [Term a 1]

-- 归一化：假设输入指数非递增，合并相邻同指数、删零系数。
mkOrd :: [Term] -> Ord
mkOrd = Ord . foldr combine [] . filter ((> 0) . tCoef)
  where
    combine t [] = [t]
    combine t (u:us)
      | tExp t == tExp u = t { tCoef = tCoef t + tCoef u } : us
      | otherwise        = t : u : us

terms :: Ord -> [Term]
terms (Ord xs) = xs

isZero :: Ord -> Bool
isZero (Ord xs) = null xs

isFinite :: Ord -> Bool
isFinite = all ((== zero) . tExp) . terms

isLimit :: Ord -> Bool
isLimit o = not (isZero o) && isLimitView o
  where isLimitView x = case view x of LimV _ -> True; _ -> False

isSuccessor :: Ord -> Bool
isSuccessor o = case view o of SuccV _ -> True; _ -> False

toNatural :: Ord -> Natural
toNatural o = case terms o of
  [Term d c] | d == zero -> c
  [] -> 0
  _  -> error "toNatural: not a finite ordinal"

suc :: Ord -> Ord
suc a = add a one

-- 比较：按首个有差异的项；该项指数大者大，指数同则比系数。
compareOrd :: Ord -> Ord -> Ordering
compareOrd (Ord xs) (Ord ys) = go xs ys
  where
    go [] [] = EQ
    go [] (_:_) = LT
    go (_:_) [] = GT
    go (x:xs') (y:ys') =
      case compareOrd (tExp x) (tExp y) of
        EQ -> case compare (tCoef x) (tCoef y) of
                EQ -> go xs' ys'
                o  -> o
        o -> o

-- 加法：β 的首项会吸收 α 中指数更小的尾巴。
add :: Ord -> Ord -> Ord
add a@(Ord xs) (Ord ys) = case ys of
  [] -> a
  y:_ -> mkOrd (takeWhile (\x -> compareOrd (tExp x) (tExp y) /= LT) xs ++ ys)

-- 乘法：按 β 的项分配，α·(ω^e·c) = (α·ω^e)·c。
mul :: Ord -> Ord -> Ord
mul a b = foldl add zero [mulTerm a y | y <- terms b]

mulTerm :: Ord -> Term -> Ord
mulTerm a (Term e c)
  | isZero a || c == 0 = zero
  | e == zero          = mulFinite a c                 -- α·有限数
  | otherwise          = mulFinite (omegaPow (add (leadExp a) e)) c
  where
    leadExp (Ord (t:_)) = tExp t
    leadExp _           = zero

mulFinite :: Ord -> Natural -> Ord
mulFinite _ 0 = zero
mulFinite a c = add (mulFinite a (c - 1)) a

-- 幂。公式：α = ω^A·m+ρ 时，α^{ω^B} = ω^(A·ω^B)；
-- 有限基底 m≥2：m^{ω^B} = ω^(ω^(B-1))（B 后继）或 ω^(ω^B)（B 极限）。
pow :: Ord -> Ord -> Ord
pow a b
  | b == zero  = one
  | a == zero  = zero
  | a == one   = one
  | isFinite b = powFinite a (toNatural b)
  | isFinite a = powFiniteBase (toNatural a) b
  | otherwise  =
      let (bb, n, sigma) = decompose b
          gamma = mul (leadExp a) (omegaPow bb)
          coreN = omegaPow (mul gamma (finite n))
      in mul coreN (pow a sigma)
  where
    leadExp (Ord (t:_)) = tExp t
    leadExp _           = zero

powFinite :: Ord -> Natural -> Ord
powFinite _ 0 = one
powFinite a c = mul a (powFinite a (c - 1))

powFiniteBase :: Natural -> Ord -> Ord
powFiniteBase m b =
  let (bb, n, sigma) = decompose b
      delta = case view bb of
                SuccV b' -> omegaPow b'
                _        -> omegaPow bb   -- bb 极限（bb=0 不会出现，b 无限）
      coreN = omegaPow (mul delta (finite n))
  in mul coreN (pow (finite m) sigma)

-- b = ω^B·n + σ
decompose :: Ord -> (Ord, Natural, Ord)
decompose (Ord []) = (zero, 0, zero)
decompose (Ord (Term bb n : rest)) = (bb, n, Ord rest)

-- 视图：后继给出前驱，极限给出标准基本列 α[n]。
data View = ZeroV | SuccV Ord | LimV (Natural -> Ord)

view :: Ord -> View
view (Ord []) = ZeroV
view o@(Ord xs)
  | tExp (last xs) == zero = SuccV (predOf o)
  | otherwise              = LimV (fund o)

predOf :: Ord -> Ord
predOf (Ord xs) = mkOrd (init xs ++ rest)
  where
    Term _ c = last xs
    rest = if c <= 1 then [] else [Term zero (c - 1)]

-- α = γ + ω^δ·c（δ > 0）
-- δ = ε+1 : α[n] = γ + ω^δ·(c-1) + ω^ε·n
-- δ 极限  : α[n] = γ + ω^(δ[n])·c
fund :: Ord -> Natural -> Ord
fund o n = mkOrd (gamma ++ mid ++ lastPart)
  where
    xs = terms o
    gamma = init xs
    Term d c = last xs
    (mid, lastPart) = case view d of
      SuccV e -> ( [Term d (c - 1) | c > 1], [Term e n] )
      LimV g  -> ( [], [Term (g n) c] )

-- 快速增长层次：f_0(n)=n+1, f_{a+1}(n)=f_a^n(n), f_a(n)=f_{a[n]}(n)（a 极限）
fgh :: Ord -> Natural -> Natural
fgh a n = case view a of
  ZeroV   -> n + 1
  SuccV b -> iterate (fgh b) n !! fromIntegral n
  LimV g  -> fgh (g n) n

-- ε0 = sup { 0, 1, ω, ω^ω, ... }，ε0 本身不属于 Ord，只给出它的基本列。
eps0Seq :: Natural -> Ord
eps0Seq n = iterate omegaPow zero !! fromIntegral n

-- 显示：ω^2·3+ω+2 风格
instance Show Ord where
  show (Ord []) = "0"
  show (Ord ts) = intercalate "+" (map showTerm ts)
    where
      showTerm (Term e c)
        | e == zero = show c
        | e == one  = "ω" ++ coefPart
        | otherwise = "ω^(" ++ show e ++ ")" ++ coefPart
        where coefPart = if c == 1 then "" else "·" ++ show c