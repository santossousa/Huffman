module Main where
import Prelude ( Ord , (<=) , (>=) , (-) , Bool ,IO , putStrLn , (<$>) , pure , (<*>))
import Functions 
import Bool

import List
import Maybe



maximum :: Ord a => [a] -> Maybe a
maximum [] = Nothing
maximum (x:xs) = Just (foldl f x xs) where
     f acc ps = cond(acc <= ps) (ps) (acc) 


maximalBy :: (a -> a -> Bool) -> [a] -> Maybe a
maximalBy cmp [] = Nothing
maximalBy cmp (x:xs) = Just (foldl f x xs) where
     f acc ps = cond(acc `cmp`  ps) (acc) (ps)

lis:: Ord a => [ a ] -> [ a ]
lis [] = []
lis [x] = [x]
lis xs = fromMaybe [] ( maximalBy cmp ( s <$> [0.. n - 1] <*> pure xs )) where
    n = length xs
    cmp ps qs = length ps >= length qs
    s 0 [] = []
    s 0 [y] = [y]
    s 0 ( y : ys ) = y : ms where
        candidates = filter (( y <=) . head ) $ s <$> [0.. k - 1] <*> pure ys

        k = length ys
        ms = fromMaybe [] $ maximalBy cmp candidates
    s i ys = s 0 $ drop i ys