module Utils where
import Produto (Produto(..))
-- separa uma string em uma lista de strings a partir de um delimitador
splitBy :: Char -> String -> [String]
splitBy _ [] = [""]
splitBy delimiter (c:cs)
    | c == delimiter = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = splitBy delimiter cs

-- Insere os atributos no arquivo csv
insereAtbs :: String -> String -> IO()
insereAtbs = writeFile

-- Copia da map
map2 :: (a -> b) -> [a] -> [b]
map2 _ [] = []
map2 f (l:ls) = f l:map2 f ls 
-- Copia da filter
filter2 :: (a -> Bool) -> [a] -> [a]
filter2 _ [] = []
filter2 f (l:ls) 
    | f l = l:filter2 f ls
    | otherwise = filter2 f ls

-- filtra a lista depois aplica uma função modificadora
filterMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterMap _ _ [] = []
filterMap f g (l:ls) 
    | f l = g l:filterMap f g ls
    | otherwise = filterMap f g ls

converterProdutoEmString :: Produto -> String
converterProdutoEmString (Produto id nome preco marca quant) = show id ++ "," ++ nome ++ "," ++ show preco ++ "," ++ marca ++ "," ++ show quant
