module Main where
import FileCompressor
import System.Environment ( getArgs )
main :: IO ()
main = do 
		 {-Yuri kamagoe freitas freire
		   Fábio José Dos Santos Sousa

		 nao deu certo fazer uma main decente 
		 essa main  e so para encode, se for testa no terminal e ./MainEncode dataRead ler o arquivo que sera decodificado
		                                                         ./MainEncode printFile aqui ele decodificado e joga no arquivo input.txt 
		 -}
         dataRead <- readFromFile "out.txt"
         printFile "inputDecode.txt" $ transform False dataRead
   
 