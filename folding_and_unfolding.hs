import Data.List (unfoldr)

-- Развернуть натуральное число n в список всех чисел, меньших его.
unfold_core n = if n<=1 then Nothing else Just (prev,  prev) where prev = pred n
unfoldN = unfoldr unfold_core

-- Развернуть число в списко разрядов его двоичного представления

binary_core 0 = Nothing
binary_core t = Just (t `mod` 2, t `div` 2)
unfoldToBinary n = reverse (unfoldr binary_core $ abs $ n)

-- Список разрядов преобразовать сверткой в значение числа

digit_core (s,t) a = (s + a * t, 2*t)
foldFromBinary l = fst (foldl digit_core (0, 1) (reverse l) )

-- Развернуть число в список его простых делителей

get_next_divider n i
    | i >= n = n
    | n `mod` i == 0 = i
    | otherwise = get_next_divider n (succ i)

prime_core (n,i)
    | n == 1 = Nothing
    | otherwise = Just (next_div, (n `div` next_div, next_div)) where next_div = get_next_divider n i

unfoldToPrimeDividers n = unfoldr prime_core (n, 2)

-- Выразить список первых n чисел Фибоначчи через развертку

fibonacci_core (a1, a2, i)
    | i == 0 = Nothing
    | otherwise = Just(a1, (a2, a1 + a2, pred i))

unfoldFibonacci n = unfoldr fibonacci_core (0, 1, n)

-- бесконечная версия
infiniteFibonacci_core (a1, a2) = Just(a1, (a2, a1 + a2))
unfoldInfiniteFibonacci = unfoldr infiniteFibonacci_core (0, 1)

-- Развернуть число в сиракузскую последовательность
collatz_core n
    | n == 0 = Nothing
    | n == 1 = Just (1, 0)
    | even n = Just (n, n `div` 2)
    | otherwise = Just (n, 3 * n + 1)

unfoldCollatz  = unfoldr collatz_core 

-- Выразить список простых чисел, не превышающих n, через развертку
-- с помощью решета Эратосфена
eratosthenes_core (n, (h:t)) = 
    if h > n then
        Nothing
    else
        Just(h,(n, (filter (\x -> x `mod` h /= 0) t)))

unfoldEratosthenes n = unfoldr eratosthenes_core (n, [3, 5..])

-- бесконечное решето
infiniteEratosthenes_core (h:t) = Just(h,(filter (\x -> x `mod` h /= 0) t))

unfoldInfiniteEratosthenes = unfoldr infiniteEratosthenes_core  [3, 5..]