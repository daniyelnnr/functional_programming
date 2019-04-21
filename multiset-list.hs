module MultisetList ()
 where

{- 
 - Um multi-conjunto (ou bag) é uma estrutura que representa uma coleção de objetos que 
 - permite duplicadas. Entretanto, as duplicatas são armazenadas como a quantidade de 
 - ocorréncias do mesmo elemento no multi-conjunto. Exemplo, a coleção {a,b,c,c,c,b} poderia 
 - ser representada como sendo {(a,1), (b,2), (c,3)}. A ideia de multi-conjunto pode ser 
 - implementada de diversas formas. Uma delas é usando a implementacao de Data.List, onde 
 - cada elemento da lista consiste do dado em si e sua quantidade (um par). 
 - Eh recomendavel que voce consulte a documentacao de Data.List
 -}
import Data.List as List

{-
 - Insere um elemento na estrutura. Caso o elemento ja existe, sua quantidade na estrutura sera incrementada.
 [('a',1), ('b',2), ('c',3)]
 -}
mslinsert elem [] = [(elem, 1)]
mslinsert elem ((x,y):xs)
  | x == elem = [(x, y+1)] ++ xs
  | otherwise = [(x,y)] ++ (mslinsert elem xs)

{-
- Remove um elemento da estrutura, levando em consideracao a manipulacao de sua quantidade na estrutura. 
- Caso a quantidade atinja 0 (ou menos), o elemento deve realmente ser removido da estrutura
-}
mslremove elem [] = error "Nao é possível remover"
mslremove elem ((x,y):xs)
  | x == elem && y == 1 = xs
  | x == elem && y > 1 = [(x,(y-1))] ++ xs
  | otherwise = [(x,y)] ++ (mslremove elem xs)

{-
 - Busca um elemento na estrutura retornando sua quantidade. Caso o elemento nao exista, retorna 0 como a quantidade.
-}
mslsearch elem [] = 0
mslsearch elem ((x,y):xs)
  | x == elem = y
  | otherwise = mslsearch elem xs

{-
 - Faz a uniao deste Bag com otherBag. A uniao consiste em ter os elementos dos dois Bags com suas maiores quantidades.
 - Por exemplo, A = [('a',1),('c',3)], B = [('b',2),('c',1)]. A.union(B) deixa A = [(a,1),(c,3),(b,2)]

 - mslunion [('a',4),('b',1)] [('d',3),('b',2),('a',2)]
-}

mslunion bag1 [] = bag1
mslunion (x:xs) (y:ys)
  | searched == 0 = mslunion ((x:xs) ++ [y]) ys
  | otherwise = if searched >= (snd y) then mslunion (x:xs) ys else mslunion ((filter (\elem -> (fst elem) /= (fst y)) (x:xs)) ++ [y]) ys
  where searched = mslsearch (fst y) (x:xs)

{-
 - Faz a intersecao deste Bag com otherBag. A intersecao consiste em ter os elementos que estao em ambos os bags com suas 
 - menores quantidades. Por exemplo, Seja A = {(a,3),(b,1)} e B = {(a,1)}. Assim, A.intersection(B) deixa A = {(a,1)}
 - Caso senhum elemento de A esteja contido em B ent�o a intersecao deixa A vazio.

 A = {(a,1),(b,1)} e B = {(a,3)}
 mslintersection [('a',4),('b',1)] [('d',3),('b',2),('a',2)]
 mslintersection [('a',3),('b',1)] [('a',1)]
-}
mslintersection bag1 [] = []
mslintersection bag1 (y:ys)
  | searched == 0 = mslintersection bag1 ys
  | otherwise = if snd y <= searched then (mslintersection (filter (\elem -> fst elem /= fst y) bag1) ys) ++ [y] else (mslintersection (filter (\elem -> fst elem /= fst y) bag1) ys) ++ [(fst y, searched)]
  where searched = mslsearch (fst y) bag1

{-
 - Faz a diferenca deste Bag com otherBag. A diferenca A \ B entre bags eh definida como segue:
   - contem os elementos de A que nao estao em B
   - contem os elementos x de A que estao em B mas com sua quantidade subtraida (qtde em A - qtde em B). 
     Caso essa quantidade seja negativa o elemento deve serremovido do Bag. 
     Por exemplo, seja A = {(a,3),(b,1)} e B = {(b,2),(a,1)}. Assim, A.minus(B) deixa A = {(a,2)}.
     
     minus [('a',3),('b',1)] [('b',2),('c',4)]
     minus [('a',3),('b',5)] [('b',2),('c',4),('a',1)]
-}
minus bag1 [] = bag1
minus [] bag2 = []
minus bag1 (y:ys)
  | searched == 0 = minus bag1 ys
  | otherwise = if searched <= snd y then minus (filter (\elem -> fst elem /= fst y) bag1) ys else minus (map (\elem -> if (fst elem) /= (fst y) then (elem) else (fst elem, (snd elem)-(snd y))) bag1) ys
  where searched = mslsearch (fst y) bag1

{-
 - Testa se este Bag esta incluso em otherBag. Para todo elemento deste bag, sua quantidade
 - deve ser menor or igual a sua quantidade em otherBag.

 inclusion [('a',3),('b',5)] [('b',2),('c',4),('a',5)]
 inclusion [('a',3),('b',1)] [('b',2),('c',4),('a',5)]
-}
inclusion bag1 [] = False
inclusion [] bag2 = True
inclusion (x:xs) bag2
  | searched == 0 = False
  | otherwise = if snd x <= searched then inclusion xs bag2 else False
  where searched = mslsearch (fst x) bag2

{-
 - Realiza a soma deste Bag com otherBag. A soma de dois bags contem os elementos dos dois bags com suas quantidades somadas. 

 mslsum [('a',3),('b',1)] [('b',2),('c',4),('a',5)]
-}
mslsum bag1 [] = bag1
mslsum bag1 (y:ys)
  | searched == 0 = mslsum bag1 ys ++ [y]
  | otherwise = mslsum (map (\elem -> if (fst elem) /= (fst y) then (elem) else (fst elem, (snd elem)+(snd y))) bag1) ys
  where searched = mslsearch (fst y) bag1

{-
 - Retorna a quantidade total de elementos no Bag
-}
mslsize [] = 0
mslsize (x:xs) = 1 + mslsize xs