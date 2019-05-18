main :: IO ()
main = return ()

--Escreva a declaracao para o tipo Triple, contendo tres elementos, todos de tipos diferentes.
--Escreva funcoes tripleFst, tripleSnd, tripleThr para extrair respectivamente o primeiro, segundo e terceiro
-- elementos de uma triple.
--data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show) 

data Triple a b c = Triple a b c deriving (Eq,Show)

tripleFst (Triple a b c) = a
tripleSnd (Triple a b c) = b
tripleThr (Triple a b c) = c

--Escreva um tipo Quadruple que contem 4 elementos: dois de um mesmo tipo e outros dois de outro tipo
--Escreva as funcoes frstTwo e secondTwo que retornam os dois primeiros e os dois ultimos, respectivamente
data Quadruple a b = Quadruple a a b b deriving (Eq,Show)

firstTwo (Quadruple a b c d) = (a,b)
secondTwo (Quadruple a b c d) = (c,d)

--Escreva um tipo de dados que pode conter um, dois, tres ou quatro elementos, dependendo do construtor
--Implemente funções tuple1 até tuple4 que que retornam Just <valor> ou Nothing se o valor nao existe
data Tuple a b c d = Tuple1 a | Tuple2 a b | Tuple3 a b c | Tuple4 a b c d deriving (Eq,Show)

tuple1 (Tuple1 a) = Just a
tuple1 (Tuple2 a b) = Just a
tuple1 (Tuple3 a b c) = Just a
tuple1 (Tuple4 a b c d) = Just a

tuple2 (Tuple2 a b) = Just b
tuple2 (Tuple3 a b c) = Just b
tuple2 (Tuple4 a b c d) = Just b
tuple2 _ = Nothing

tuple3 (Tuple3 a b c) = Just c
tuple3 (Tuple4 a b c d) = Just d
tuple3 _ = Nothing

tuple4 (Tuple4 a b c d) = Just d
tuple4 _ = Nothing

--Implementação da lista
data List a = Nil | Cons a (List a) deriving (Eq,Show)

listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs

listHead Nil = error "Empty list"
listHead (Cons x xs) = x

listTail Nil = error "Empty list"
listTail (Cons x xs) = xs

listFoldr f v Nil = v
listFoldr f v (Cons x xs) = f x (listFoldr f v xs)

listFoldl f v Nil = v
listFoldl f v (Cons x xs) = listFoldl f (f v x) xs 

--Escreva as funcoes sobre a estrutura de dados binary tree
--Node 5 (Node 1 NIL NIL) (Node 3 NIL NIL)
data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a) deriving (Eq,Show)

value NIL = 0
value (Node a left right) = a

-- Size of a tree is the number of elements present in the tree
sizeBST NIL = 0
sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right

--verifica se uma BT é uma BST
isBST NIL = True
isBST (Node _ NIL NIL) = True
isBST (Node a left right)
  | a < value left = False
  | a > value right = False
  | otherwise = isBST left && isBST right

--insere uma nova chave na BST retornando a BST modificada
insert x NIL = Node x NIL NIL
insert x (Node a left right)
  | x < a = Node a (insert x left) right
  | x > a = Node a left (insert x right)
  | otherwise = Node x left right

--retorna o Node da BST contendo o dado procurado ou entao NIL
search x NIL = NIL
search x (Node a NIL NIL)
  | x == a = Node a NIL NIL
  | otherwise = NIL
search x (Node a left right)
  | x == a = Node a left right
  | x < a = search x left
  | x > a = search x right

--retorna o elmento maximo da BST
maxBST NIL = -1
maxBST (Node a NIL NIL) = a
maxBST (Node a _ right) = maxBST right

--retorna o elemento minimo da BST
minBST NIL = -1
minBST (Node a NIL NIL) = a
minBST (Node a left _) = minBST left

--check elem is in BST
isElem x NIL = False
isElem x bst = value (search x bst) == x

toList NIL = []
toList (Node a left right) = (toList left) ++ [a] ++ (toList right)

-- let b = (Node 5 (Node 3 (Node 1 NIL NIL) (Node 4 NIL NIL)) (Node 7 (Node 6 NIL NIL) (Node 8 NIL NIL)))
-- let bst = (Node 15 (Node 6 (Node 3 (Node 2 NIL NIL) (Node 4 NIL NIL)) (Node 7 NIL (Node 13 (Node 9 NIL NIL) NIL))) (Node 20 NIL NIL))

--retorna o predecessor de um elemento da BST, caso o elemento esteja na BST
predecessor x NIL = error "Nao existe predecessor"
predecessor x (Node a NIL NIL) = error "Nao existe predecessor para esse elemento"
predecessor x bst
  | x == minBST bst = error "Nao existe predecessor para esse elemento"
  | isElem x bst == False = error "Elemento nao esta na BST"
  | otherwise = search (predecessorAux x (toList bst)) bst

-- retorna key do node predecessor
predecessorAux elem (x:xs)
  | (not (null xs)) && (elem == head xs) = x
  | otherwise = predecessorAux elem xs

--retorna o sucessor de um elemento da BST, caso o elemento esteja na BST
successor x NIL = error "Nao existe sucessor"
successor x (Node a NIL NIL) = error "Nao existe sucessor para esse elemento"
successor x bst
  | x == maxBST bst = error "Nao existe sucessor para esse elemento"
  | isElem x bst == False = error "Elemento nao esta na BST"
  | otherwise = search (successorAux x (reverse (toList bst))) bst

successorAux elem (x:xs)
  | (not (null xs)) && (elem == head xs) = x
  | otherwise = successorAux elem xs

--remove ume lemento da BST
remove = undefined

--retorna uma lista com os dados da BST nos diversos tipos de caminhamento
preOrder = undefined
order = undefined
postOrder = undefined

nodeParent e NIL = NIL
nodeParent e (Node a left right)
  | isElem e (Node a left right) == False = NIL
  | e == a = NIL
  | e == (value left) || e == (value right) = Node a left right
  | otherwise = if e < a then nodeParent e left else nodeParent e right