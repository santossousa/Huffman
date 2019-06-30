module FileCompressor (
  
    readFromFile,
    printFile,
    transform
)
where

import Huffman
import Data.Word
import Data.List
import qualified Data.ByteString as BS

type Byte = Data.Word.Word8

readFromFile :: String -> IO [Byte]
readFromFile file = BS.readFile file >>= return . BS.unpack

printFile :: String -> [Byte] -> IO ()
printFile file = BS.writeFile file . BS.pack

bitsToBytes :: [Bool] -> [Byte]
bitsToBytes list = reverse $ bitsABytes' [] list 0 0
    where
        bitsABytes' all []           acc 8 = acc:all
        bitsABytes' all []           acc n = bitsABytes' all       []   (acc*2)   (n+1)
        bitsABytes' all list         acc 8 = bitsABytes' (acc:all) list 0         0
        bitsABytes' all (False:list) acc n = bitsABytes' all       list (acc*2)   (n+1)
        bitsABytes' all (True:list)  acc n = bitsABytes' all       list (acc*2+1) (n+1)



byteToBits :: Byte -> [Bool]
byteToBits = reverse . byteToBits' 8
    where
        byteToBits' 0 _ = []
        byteToBits' n x
            | even x    = False : byteToBits' (n-1) (div x 2)
            | otherwise = True  : byteToBits' (n-1) (div x 2)
{-
Codifica uma árvore de Huffman em uma representação binária.
  Codificação:
  Se um Verdadeiro for lido, existem duas possibilidades. Se é o primeiro Verdadeiro que é lido
  (no início do código ou depois de um falso), então o próximo bit
  faz parte da árvore (representada inversamente). Caso contrário, é considerado
  finalizou o código. Se um falso é lido, então os próximos 8 bits,
  correspondem a um símbolo.


-}
encodeHuffman :: Huffman Byte -> [Bool]
encodeHuffman h = (concatMap encodeAssociations assocs) ++ [True, False]
    where
        assocs = generateAssociations h
        encodeAssociations (e, cods)
            = (True : intersperse True (reverse cods)) ++ (False : byteToBits e)
{-

Ler uma codificação de uma árvore de Huffman e reconstrua a
  Árvore da qual foi gerado, separando o resto dos dados binários.
-}
parseHuffman :: [Bool] -> (Huffman Byte, [Bool])
parseHuffman code = (huff, trim rest)
    where
        (assocs, rest) = parseAssocsInit [] code
        huff           = reconstructHuffman assocs
        trim           = reverse . tail . dropWhile not . reverse
        parseAssocsInit acc (True:False:xs) = (acc, xs)
        parseAssocsInit acc (True:True:xs)  = parseAssocs acc [True] xs
        parseAssocs acc cod (False:xs)      = parseAssocsInit ((head $ bitsToBytes e, cod):acc) r
            where (e, r) = splitAt 8 xs
        parseAssocs acc cod (True:x:xs)     = parseAssocs acc (x:cod) xs
{-

Transforma uma lista de bytes em outra, comprimindo ou descompactando
  conforme especificado (True é compactar, False é descompactar).
  Ao compactar, o último True é adicionado para ter um caractere oa final de arquivo
  (Todos os Falso encontrados antes do último Verdadeiro serão ignorados pelo decodificador)
-}
transform :: Bool -> [Byte] -> [Byte]
transform True code = bitsToBytes $ encodeHuffman huff ++ cods ++ [True]
    where
        (huff, cods) = encode code
transform _ code = decode huff cods
    where
        (huff, cods) = parseHuffman . concat $ map byteToBits code
