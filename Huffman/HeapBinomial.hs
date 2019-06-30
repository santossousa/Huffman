module HeapBinomial (
    
    module HeapBinomial) 
where

data BinomialTree a = Nodo Int a [BinomialTree a] deriving (Show, Read)

data HeapBinomial a = BH [BinomialTree a] deriving (Show, Read)

height :: BinomialTree a -> Int
height (Nodo o e h) = o


root :: BinomialTree a -> a
root (Nodo o e h) = e  

{-
Dadas duas árvores de ordem r retorna uma árvore de ordem r + 1
     com os mesmos elementos das árvores da ordem r


-}
ordemTree :: (Ord a) => BinomialTree a -> BinomialTree a -> BinomialTree a
ordemTree t1@(Nodo r x1 c1) t2@(Nodo _ x2 c2)
    | x1 <= x2 = Nodo (r+1) x1 (t2:c1)
    | otherwise = Nodo (r+1) x2 (t1:c2)

insTree :: (Ord a) => BinomialTree a -> [BinomialTree a] -> [BinomialTree a] 
insTree t [] = [t]
insTree t (x:xs) = if height t < height x then t:(x:xs) else insTree (ordemTree t x) xs
 

insert :: (Ord a) => a -> HeapBinomial a -> HeapBinomial a
insert x (BH ts) = BH (insTree (Nodo 0 x []) ts)


mrg :: (Ord a) => [BinomialTree a] -> [BinomialTree a] -> [BinomialTree a]
mrg ts1 [] = ts1 
mrg [] ts2 = ts2
mrg ts1@(t1:ts1') ts2@(t2:ts2') 
    | height t1 < height t2 = t1 : mrg ts1' ts2
    | height t2 < height t1 = t2 : mrg ts1 ts2'
    | otherwise = insTree (ordemTree t1 t2) (mrg ts1' ts2')

--funcao auxiliar
removeMinTree :: (Ord a) => [BinomialTree a] -> (BinomialTree a, [BinomialTree a])
removeMinTree [] = error "Heap vazia"
removeMinTree [t] = (t, [])
removeMinTree (t:ts) = if root t < root t' then (t, ts) else (t',t:ts')
    where (t',ts') = removeMinTree ts

--minimo
findMin :: (Ord a) => HeapBinomial a -> a
findMin (BH ts) = root t
    where (t,_) = removeMinTree ts
  
  
deleteMin :: (Ord a) => HeapBinomial a -> HeapBinomial a
deleteMin (BH ts) = BH (mrg (reverse ts1) ts2)
    where (Nodo _ x ts1, ts2) = removeMinTree ts
