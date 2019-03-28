 {- Usando os predicados not,and e or prontos de Haskell, implemente os predicados (funcoes) xor (or exclusivo),
  - impl (implicacao A => B é equivalente a (not A or B)) e equiv (A <=> B é definido como A => B and B => A)
  - Procure usar casamento de padroes e reutilizar as funcoes.
  -}

main :: IO ()
main = return ()

xor True True = False
xor False False = False
xor _ _ = True

{-
impl True False = False
impl _ _ = True
-}

impl a b = not a || b

equiv a b = impl a b && impl b a

{-
- Implemente a funcao potencia, que retorna o resultado de x elevado a y 
-}
pow x 0 = 1
pow x y = x * pow x (y-1)

{-
- Implemente a funcao fatorial que calcula o fatorial de um numero 
-}
fatorial 0 = 1
fatorial 1 = 1
fatorial x = x * fatorial (x-1)

{-
- Determina se um numero eh primo ou nao. Preocupe-se apenas em resolver o problema.
- Nao precisa usar conhecimentos mais sofisticados da teoria dos numeros. Voce pode trabalhar com listas.
- na lista de naturais, se o numero X não for divisiível por zero (x mod num /= 0)
-}

isPrime k = null [ x | x <- [2..k - 1], k `mod` x == 0]

{-
- Calcula um termo da sequencia de Fibonnacci. Voce pode trabalhar com listas. 
-}
fib 0 = 0
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)

{-
- Calcula um MDC de dois numeros usando o algoritmo de Euclides. 
-}
mdc x 0 = x
mdc x y = mdc a b
  where a = y
        b = mod x y

{-
- Calcula um MMC de dois numeros. 
-}
mmc x y = (x * y) `div` (mdc x y)

{-
- Determina se dois numeros inteiros positivos sao co-primos. Dois numeros sao co-primos se 
- o mdc deles for igual a 1. Ex: coprimo 35 64 = True 
-}
coprimo x y
  | mdc x y == 1 = True
  | otherwise = False

{-
- Calcula a conjectura de Goldbach, que diz que um numero par maior que 2 pode ser escrito como a soma de dois numeros primos. Ex: 28 = 5 + 23.
-}
gold x = undefined

primes x = [a | a <- [1..x], isPrime a, a > 2]
