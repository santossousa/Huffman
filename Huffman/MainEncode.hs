module Main where
import FileCompressor
import System.Environment ( getArgs )
main :: IO ()
main = do 
		 {-
		 nao deu certo fazer uma main decente 
		 essa man  e so para encode, se for testa no terminal e ./MainEncode dataRead ler o arquivo que sera codificado
		                                                         ./MainEncode printFile aqui ele codifica e joga no arquivo out.txt 
		 -}
         dataRead <- readFromFile "input.txt"
         printFile "out.txt" $ transform True dataRead
   
 