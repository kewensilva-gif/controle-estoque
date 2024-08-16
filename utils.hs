module Utils where
import Produto (Produto(..))
import GHC.IO.IOMode (IOMode(WriteMode, AppendMode, ReadMode))

import GHC.IO.Handle (hClose, hPutStr, hGetContents)
import GHC.IO.Handle.FD (withFile)
import Text.Read (readMaybe)

-- separa uma string e transforma em uma lista de strings a partir de um delimitador
splitBy :: Char -> String -> [String]
splitBy _ "" = []
splitBy delim str = let (before, remainder) = break (== delim) str
                     in before : case remainder of
                                    [] -> []
                                    _:after -> splitBy delim after

-- função auxiliar para extrair e converter o ID da linha
parseId :: String -> Maybe Int
parseId linha = case splitBy ',' linha of
                    (idStr:_) -> readMaybe idStr
                    [] -> Nothing

-- Insere os atributos no arquivo csv
insereAtbs :: FilePath -> String -> IO ()
insereAtbs caminho atbs = do
    withFile caminho WriteMode $ \arquivo -> do
        hPutStr arquivo atbs

-- Cópia da map
map2 :: (a -> b) -> [a] -> [b]
map2 _ [] = []
map2 f (l:ls) = f l:map2 f ls 
-- Cópia da filter
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

-- auto-incrementa o id
idAutoIncrement :: FilePath -> IO Int
idAutoIncrement caminho = withFile caminho ReadMode $ \arquivo -> do
    conteudo <- hGetContents arquivo
    let linhas = lines conteudo

    length conteudo `seq` return ()
    
    -- pega última linha e extrai o id
    let numero = case linhas of
            [] -> 0 
            _  -> case parseId (last linhas) of
                    Just n  -> n  -- se a conversão for funcionar
                    Nothing -> 0  -- se a falhar
    return (numero + 1)