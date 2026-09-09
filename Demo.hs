import Prelude hiding (Ord)
import OrdinalPA

check :: String -> Bool -> IO ()
check name ok = putStrLn (name ++ ": " ++ (if ok then "OK" else "FAIL"))

w2 :: Ord
w2 = omegaPow (finite 2)

wW :: Ord
wW = omegaPow omega

main :: IO ()
main = do
  putStrLn "== 吸收律 =="
  check "1+ω = ω"                  (add one omega == omega)
  check "2·ω = ω"                  (mul (finite 2) omega == omega)
  check "(ω+1)+ω = ω+(1+ω) = ω·2"  (add (add omega one) omega == mul omega (finite 2))

  putStrLn "\n== 非交换 =="
  check "ω+1 ≠ 1+ω"                (add omega one /= add one omega)
  check "ω·2 ≠ 2·ω"                (mul omega (finite 2) /= mul (finite 2) omega)

  putStrLn "\n== 展开 =="
  check "(ω+1)·2 = (ω+1)+(ω+1) = ω+(1+ω)+1 = ω·2+1"
        (mul (add omega one) (finite 2) == add (mul omega (finite 2)) one)
  check "(ω+1)^2 = ω^2+ω+1"
        (pow (add omega one) (finite 2) == add (add w2 omega) one)

  putStrLn "\n== 幂 =="
  check "2^ω = ω"                  (pow (finite 2) omega == omega)
  check "2^(ω^2) = ω^ω"            (pow (finite 2) w2 == wW)
  check "(ω+1)^(ω+1) = ω^(ω+1) + ω^ω"    (pow (add omega one) (add omega one) == add (omegaPow (add omega one)) wW)

  putStrLn "\n== 比较链 =="
  let chain = [ finite 3, omega, add omega one, mul omega (finite 2)
              , w2, wW, eps0Seq 4 ]
  check "3 < ω < ω+1 < ω·2 < ω^2 < ω^ω < ε0[4]"
        (all (== LT) (zipWith compareOrd chain (tail chain)))

  putStrLn "\n== 基本列 =="
  check "ω[n] = n"                 (fund omega 5 == finite 5)
  check "(ω·2)[n] = ω+n"           (fund (mul omega (finite 2)) 5 == add omega (finite 5))
  check "(ω^2)[n] = ω·n"           (fund w2 5 == mul omega (finite 5))
  check "(ω^ω)[n] = ω^n"           (fund wW 3 == omegaPow (finite 3))
  check "(ω^2+ω)[n] = ω^2+n"       (fund (add w2 omega) 4 == add w2 (finite 4))

  putStrLn "\n== 视图 =="
  putStrLn ("view (ω+1) = " ++ showView (view (add omega one)))
  putStrLn ("view (ω·2) = " ++ showView (view (mul omega (finite 2))))

  putStrLn "\n== FGH（小参数）=="
  check "f_0(5) = 6"     (fgh zero 5 == 6)
  check "f_1(5) = 10"    (fgh one 5 == 10)
  check "f_2(5) = 160"   (fgh (finite 2) 5 == 160)
  check "f_3(2) = 2048"  (fgh (finite 3) 2 == 2048)
  check "f_ω(2) = f_2(2) = 8" (fgh omega 2 == 8)
  putStrLn "（f_ω(3)=f_3(3) 已经太大，不运行）"

  putStrLn "\n== ε0 的基本列 =="
  mapM_ (\n -> putStrLn ("ε0[" ++ show n ++ "] = " ++ show (eps0Seq n))) [0..4]

showView :: View -> String
showView ZeroV    = "0"
showView (SuccV b) = "Succ(" ++ show b ++ ")"
showView (LimV _)  = "Lim(α[n])"