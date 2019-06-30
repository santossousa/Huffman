module Huffman (
    
    Huffman,
    
    frequency,
    createBranch,
    createHuffman,
    generateAssociations,
    reconstructHuffman,
    encode,
    decode
)
where

import HeapBinomial as BHeap


data Huffman a = Leaf Integer a 
               | Branch Integer (Huffman a) (Huffman a)
               deriving (Show, Read)


instance (Eq a) => Eq (Huffman a) where
    (Leaf f e)        == (Leaf f2 e2)         = (f == f2) && (e == e2)
    (Branch frec i d) == (Branch frec2 i2 d2) = (frec == frec2) && (i == i2) && (d == d2)
    _ == _ = False 


instance (Ord a) => Ord (Huffman a) where
    Leaf f e     <= Leaf f2 e2      = f <= f2
    Branch f i d <= Leaf f2 e       = f <= f2
    Leaf f e     <= Branch f2 i d   = f <= f2
    Branch f i d <= Branch f2 i2 d2 = f <= f2

{-

Obtém a freqüência acumulada em uma raiz da árvore Huffman
-}
frequency :: Huffman a -> Integer
frequency (Leaf f _)     = f
frequency (Branch f _ _) = f
{-
Dadas duas árvores Huffman, ela retorna uma nova com as originais
   como seus filhos. A frequência acumulada da árvore resultante é a soma
   da freqüência de seus filhos.

-}

createBranch :: Huffman a -> Huffman a -> Huffman a
createBranch l r = Branch (frequency l + frequency r) l r

{-
Dada uma lista de elementos com suas frequências correspondentes constrói um
   BinomialHeap de árvores Huffman que contém os ditos elementos junto com seus
   freqüências

-}

createSet :: (Ord a) => [(a, Integer)] -> HeapBinomial (Huffman a)
createSet = foldl add (BH []) 
    where
        add acc (elem, freq) = BHeap.insert (Leaf freq elem) acc
{-
Dada uma lista que contém um conjunto de pares de símbolo e ocorrência, constroi uma árvore de Huffman que representa tal conjunto


-}

createHuffman :: (Ord a) => [(a, Integer)] -> Huffman a
createHuffman l = createHuffman' (createSet l) 
{-
Dado um conjunto de árvores Huffman representadas por um HeapBinomial, retorna
   uma única árvore de Huffman com os valores de todas as árvores iniciais
-}
createHuffman' :: (Ord a) => HeapBinomial (Huffman a) -> Huffman a
createHuffman' (BH [(Nodo 0 t _)]) = t
createHuffman' c = createHuffman' (BHeap.insert newBranch heapWithoutAB)
    where
        branchA       = findMin c
        branchB       = findMin $ deleteMin c
        newBranch     = createBranch branchA branchB
        heapWithoutAB = deleteMin $ deleteMin c
{-

Dado uma árvore de Huffman, retorne uma lista de tuplas com o conjunto
   de todos os símbolos que ocorrem na árvore, ao lado da codificação
   binário deles (A lista booleana representa a lista de bits
   da codificação onde False representa zero e True o um)
-}
generateAssociations :: (Ord a) => Huffman a -> [(a, [Bool])]
generateAssociations a = generateAssociations' a [] []


generateAssociations' :: (Ord a) => Huffman a -> [(a, [Bool])] -> [Bool] -> [(a, [Bool])]
generateAssociations' (Leaf _ elem)  assoc acc = (elem, acc ++ [True]):assoc
generateAssociations' (Branch _ i d) assoc acc = lefts ++ rights
    where 
        lefts  = generateAssociations' i assoc (acc ++ [False]) 
        rights = generateAssociations' d assoc (acc ++ [True])

{-
Dada uma lista de tuplas com um conjunto de símbolos, ao lado de
   codificação binária deles (A lista booleana representa
   a lista de bits da codificação, em que False representa zero
   e True o um). Construa uma Huffman Tree de tal forma que essas associações
   eles poderiam ter sido gerados. Para preservar as propriedades de um bom
   Huffman, a frequência acumulada será atribuída a zero (0) para
   todas as folhas e galhos dela

-}
reconstructHuffman :: [(a, [Bool])] -> Huffman a
reconstructHuffman ((e,[True]):[]) = Leaf 0 e
reconstructHuffman l               = Branch 0 (reconstructHuffman left) (reconstructHuffman right)
    where
        left =  [(e,xs)| (e,(False:xs)) <- l]
        right = [(e,xs)| (e,(True:xs))  <- l]

{-
Dada uma lista de símbolos, retorna uma lista de tuplas onde o primeiro valor de cada
   tupla é um símbolo da lista original e o segundo valor é o número
   de ocorrências desse símbolo na lista original.

-}
countOccurrences :: (Eq a) => [a] -> [(a,Integer)] -> [(a, Integer)]
countOccurrences [] acc = acc
countOccurrences (x:xs) acc = countOccurrences rest ((x, 1 + repet):acc)
    where 
        rest = [e | e <- xs, e /= x]
        repet = sum [1 | a <- xs, a == x]
{-

Dada uma lista de símbolos, retorne uma tupla. Esta tupla tem como
   primeiro elemento do Huffman Tree gerado a partir da lista de
   dado símbolos. Como segundo elemento, tem uma lista onde cada um
   dos símbolos na entrada, foi substituído por sua representação
   binário e concatenado para formar uma única cadeia.
-}

encode :: (Ord a) => [a] -> (Huffman a, [Bool])
encode l = (tree, concatMap getCode l)
    where 
        tree      = createHuffman $ countOccurrences l []
        codes     = generateAssociations tree
        getCode s = snd (head (dropWhile (\(e,c) -> e/=s) codes))



encode' :: (Ord a) => [a] -> Huffman a
encode' l = (tree)
    where 
        tree      = createHuffman $ countOccurrences l []
        codes     = generateAssociations tree
        getCode s = snd (head (dropWhile (\(e,c) -> e/=s) codes))







decode'::(Ord a) => Huffman a -> [(a,[Bool])]
decode' b = generateAssociations b


--decode'' :: [(a,[Bool])] -> [[Bool]]
--decode'' [] = []
--decode'' [(a,b)] = 
    
{-
Dada uma árvore de Huffman e uma lista que representa uma cadeia de bits, retorna uma lista de símbolos que corresponde à decodificação da cadeia com as informações das árvores.

-}
decode :: Huffman a -> [Bool] -> [a]
decode a [] = []
decode a c  = getSymbol a c []
    where
        getSymbol (Leaf _ e)     []         acc = acc
        getSymbol (Leaf _ e)     (_:rest)   acc = acc ++ [e] ++ (decode a rest)
        getSymbol (Branch _ _ d) (True:xs)  acc = getSymbol d xs acc
        getSymbol (Branch _ i _) (False:xs) acc = getSymbol i xs acc



--byteToBits :: [a] -> [Bool]
--byteToBits = reverse . byteToBits' 8
  --  where
     --   byteToBits' 0 _ = []
       -- byteToBits' n x
         --   | even x    = False : byteToBits' (n-1) (div x 2)
           -- | otherwise = True  : byteToBits' (n-1) (div x 2)


--isValid ::(Ord a) =>  Huffman a ->[Bool]
--isValid b =  concatMap  b  getCode where
  --  code = decode' b
    --getCode s = snd (head (dropWhile (\(e,c) -> e/=s) code))






myIf :: (Eq a) => [a] -> [a] -> Bool
myIf a b = a == b
--myIf (x:xs) (y:ys) = (x == y) && (tail(xs) == tail(ys))

inits' :: [a]  -> [[a]]
inits' [] = [[]]
inits' xs = inits' (init xs) ++ [xs]
