main :: IO ()
main = return ()

square = \x -> x*x

--Implemente as funções anteriormente escritas usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes
pow = \x y -> if y == 0 then 1 else x * pow x (y-1)

fatorial = \x -> if x < 2 then 1 else x * fatorial (x-1)

isPrime = \x -> isPrime' x [2..x-1]
isPrime' = \x y -> null (filter (\n -> mod x n == 0) y)

fib = \x -> if x < 2 then x else fib (x-1) + fib (x-2)

mdc = \x y -> if y == 0 then x else mdc y (mod x y)

mmc = \x y -> div (x * y) (mdc x y)

coprimo = \x y -> if mdc x y == 1 then True else False

goldbach x = undefined

--Implemente as funções sobre listas escritas previsamente usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes
meuLast = \xs -> if null xs then error "Lista vazia!" else if (length xs == 1) then (head xs) else meuLast (tail xs)

penultimo = \xs -> if null xs then error "Lista sem penultimo" else if (meuLast (xs) == (head xs)) then error "Lista sem penultimo" else meuLast (init xs)

elementAt i xs = undefined

meuLength xs = undefined

meuReverso xs = undefined

isPalindrome xs = undefined

compress xs = undefined

compact xs = undefined

encode xs = undefined

split xs i = undefined

slice xs imin imax = undefined

insertAt el pos xs = undefined

sort xs = undefined

mySum xs = undefined

maxList xs = undefined

buildPalindrome xs = undefined

mean xs = undefined

myAppend xs ys = undefined
