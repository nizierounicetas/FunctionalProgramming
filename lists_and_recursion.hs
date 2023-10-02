import Control.Monad (join)
myGcd a b 
    | a > b = myGcd (a - b) b
    | b > a = myGcd a (b - a)
    | otherwise = a
--------------------------------------
binaryPow _ 0 = 1
binaryPow x k = if odd k then x * binaryPow x (pred k)
     else binaryPow (x * x)  $ div k 2
--------------------------------------
matrix2Mult l1 l2 = [l1!!0 * l2!!0 + l1!!1 * l2!!2, l1!!0 * l2!!1 + l1!!1 * l2!!3, l1!!2 * l2!!0 + l1!!3 * l2!!2, l1!!2 * l2!!1 + l1!!3 * l2!!3]

getFibHelper _ 0 = [1, 0, 0, 1]
getFibHelper l n = if odd n then matrix2Mult (getFibHelper l (pred n)) l else getFibHelper (matrix2Mult l l)  (n `div` 2)
getFib n = last $ getFibHelper [0, 1, 1, 1] $ pred n

--------------------------------------
getDivisorsExceptHepler x n l
    | n * 2 > x = l 
    | x `mod` n == 0 = getDivisorsExceptHepler x  (succ n) (n:l)
    | otherwise = getDivisorsExceptHepler x (succ n) l

getDivisorsBelow x = getDivisorsExceptHepler x 1 []

isPerfect x = x == sum (getDivisorsBelow x)
--------------------------------------
collatzHelper a n
    | even a = collatzHelper (div a 2) (succ n)
    | a == 1 = succ n
    | otherwise = collatzHelper (3 * a + 1) (succ n)

collatz a = collatzHelper a 0
--------------------------------------

delannoy 0 _ = 1
delannoy _ 0 = 1
delannoy m n = delannoy (m - 1) n + delannoy m (n - 1) + delannoy (m - 1) (n - 1)

---
delannoyHepler m n i l
    | m * n == i = l
    | i `mod` n == 0 || i `div` n == 0 = delannoyHepler m n (succ i) (l ++ [1])
    | otherwise = delannoyHepler m n (i + 1) (l ++ [l!!(i - 1) + l!!(i - n) + l!!(i - n - 1)])

delannoy2 m n = last (delannoyHepler (succ m)  (succ n) 0 [])

---
delannoyHelper3 m n l1 l2 i
    | m * n == i = l2
    | i `div` n == 0 = delannoyHelper3 m n [] (l2 ++ [1]) (succ i)
    | i `mod` n == 0 = delannoyHelper3 m n l2 [1] (succ i)
    | otherwise = delannoyHelper3 m n l1 (l2 ++ [l2!!(j - 1) + l1!!j + l1!!(j - 1)]) (succ i)
    where j = i `mod` n

delannoy3 m n = last (delannoyHelper3 (succ m)  (succ n) [] [] 0)
--------------------------------
evalPolynomialHepler s [] _ = s
evalPolynomialHepler s cs x = evalPolynomialHepler (s + last cs) (map (x *) (init cs)) x

evalPolynomial = evalPolynomialHepler 0
--------------------------------

clone n l = join (map (replicate n ) l)

--------------------------------
xZipWithHepler f l [] l2 = l
xZipWithHepler f l l1 [] = l
xZipWithHepler f l (h1:t1) (h2:t2) = xZipWithHepler f (l ++ [f h1 h2]) t1 t2
xZipWith f = xZipWithHepler f []
--------------------------------

--------------------------------
fromDigitsHelper n s c [] = s
fromDigitsHelper n s c l =  fromDigitsHelper n (s + (c * last l)) (c * n) (init l)

fromDigits n = fromDigitsHelper n 0 1 

---
toDigitsHelper n 0 l = l
toDigitsHelper n c l = toDigitsHelper n (c `div` n) ((c `mod` n):l)
toDigits n t = toDigitsHelper n t []
---

addDigitwise n l1 l2 = toDigits n (fromDigits n l1 + fromDigits n l2)

--
addDigitwiseHelper n [] [] l t = if t == 0 then l else t:l
addDigitwiseHelper n [] l2 l t = let r = (t + last l2) in addDigitwiseHelper n [] (init l2) ((r `mod` n):l) (r `div` n)
addDigitwiseHelper n l1 [] l t = let r = (t + last l1) in addDigitwiseHelper n [] (init l1) ((r `mod` n):l) (r `div` n)
addDigitwiseHelper n l1 l2 l t = let r = (t + last l1 + last l2) in addDigitwiseHelper n (init l1) (init l2) ((r `mod` n):l) (r `div` n)

addDigitwise2 n l1 l2 = addDigitwiseHelper n l1 l2 [] 0
-------------------------------
fibHelper a 1 = a
fibHelper a i = zipWith (+) a (tail $ fibHelper a (pred i))

universalFibonacciList [] = []
universalFibonacciList l = f where f = l ++ fibHelper f (length l)

-------------------------------
delannoyPathsHelper m n l1 l2 i
    | m * n == i = l2
    | i `div` n == 0 = delannoyPathsHelper m n [] (l2 ++ [[last (last l2) ++ [0] ]]) (succ i)
    | i `mod` n == 0 = delannoyPathsHelper m n l2 [[head ( head l2) ++ [2]]] (succ i)
    | otherwise = delannoyPathsHelper m n l1 (l2 ++ [join [map (\l -> l ++ [0]) (l2!!(j - 1)), map(\l -> l ++ [2]) (l1!!j), map(\l -> l ++ [1]) (l1!!(j - 1))]]) (succ i) where j = i `mod` n
delannoyPaths m n = last (delannoyPathsHelper (succ m)  (succ n) [] [[[]]] 0)
