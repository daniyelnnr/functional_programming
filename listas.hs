main :: IO ()
main = return ()

{-
- Encontra o ultimo elemento de uma lista. Caso a lista seja vazia retorne o seguinte comando: error "Lista vazia!" 
-}
meuLast xs = meuLast' xs
meuLast' [] = error "Lista vazia!"
meuLast' (x:xs)
  | xs == [] = x
  | otherwise = meuLast xs

{-
- Encontra o penultimo elemento de uma lista. Caso a lista seja vazia ou tenha apenas um elemento retorne o seguinte comando: error "Lista sem penultimo" 
-}
penultimo xs = penultimo' xs
penultimo' [] = error "Lista sem penultimo"
penultimo' (x:xs)
  | meuLast (x:xs) == x = error "Lista sem penultimo"
  | otherwise = meuLast (init (x:xs))

{-
- Retorna o k-esimo (k varia de 1 ate N) elemento de uma lista. Ex: elementAt 2 [4,7,1,9] = 7
-}
elementAt i xs = xs !! (i - 1)

{-
- Retorna o tamanho de uma lista. 
-}
meuLength xs = len xs
len [] = 0
len (x:xs) = 1 + len xs

{-
- Retorna o inverso de uma lista. 
-}
meuReverso xs = rev xs
rev [] = []
rev (x:xs) = rev xs ++ [x]

{-
- Diz se uma lista é palindrome. 
-}
isPalindrome xs = if xs == rev xs then True else False

{-
- Remove os elementos duplicados de uma lista. Ex: compress [2,5,8,2,1,8] = [2,5,8,1]
- Voce pode usar a funcao elem de Haskell
-}
{-
removeElem e [] = []
removeElem e xs = filter (\n -> n /= e) xs
compress xs = compress' xs
compress' [] = []
compress' (x:xs)
  | x == head xs = removeElem
  x == head xs = compress (x ++ tail xs)
  | otherwise = compress (x:xs)
-}
compress [] = []
compress xs
  | elem (last xs) (init xs) = compress (init xs)
  | otherwise = compress (init xs) ++ [last xs]

{-
- Varre a lista da esquerda para a direita e junta os elementos iguais. Ex: compact [2,5,8,2,1,8] = [2,2,5,8,8,1]
- Voce pode usar funcoes sobre listas como : (cons), filter, etc.
-}
compact [] = []
compact xs
  | elem (last xs) (init xs) = compact (init xs)
