module Estoque where
import Produto ( Produto(..) )
-- import GHC.List
import System.IO
import Utils (filter2, splitBy, filterMap)
import Data.List (isPrefixOf)

-- Declaração de atributos
type Quant = Int
type IdProd = Int
type Estoque = [Produto]
listaProdutos :: Estoque
listaProdutos = [ 
        Produto 0 "Queijo" 1.4 "Natville" 5,
        Produto 1 "Leite" 2.0 "Italac" 10,
        Produto 2 "Pao" 0.5 "Panificadora" 15,
        Produto 3 "Cafe" 3.2 "Melitta" 8,
        Produto 4 "Acucar" 1.8 "Uniao" 12,
        Produto 5 "Manteiga" 2.5 "Aviacao" 7,
        Produto 6 "Arroz" 4.0 "Tio Joao" 20,
        Produto 7 "Feijao" 3.0 "Camil" 18,
        Produto 8 "Macarrao" 2.3 "Galo" 9,
        Produto 9 "Oleo" 3.5 "Liza" 6
    ]

-- Métodos

criarEstoque :: [Produto] -> IO()
criarEstoque [] = do putStrLn "Estoque criado com sucesso!\n"
criarEstoque (p:ps) = do
    registrarProduto p
    criarEstoque ps
    

getNomeProduto :: Estoque -> IdProd -> String
getNomeProduto [] _ = "Produto Inexistente"
getNomeProduto (Produto id nome _ _ _:ps) idSearch
    | id == idSearch = nome
    | otherwise = getNomeProduto ps idSearch

getPrecoProduto :: Estoque -> IdProd -> Float
getPrecoProduto [] _ = 0.0
getPrecoProduto (Produto id _ preco _ _:ps) idSearch
    | id == idSearch = preco
    | otherwise = getPrecoProduto ps idSearch

getMarcaProduto :: Estoque -> IdProd -> String
getMarcaProduto [] _ = "Produto Inexistente"
getMarcaProduto (Produto id _ _ marca _:ps) idSearch
    | id == idSearch = marca
    | otherwise = getMarcaProduto ps idSearch

-- adiciona produto ao estoque
registrarProduto:: Produto -> IO()
registrarProduto (Produto id nome preco marca qtd) = do
    arq <- openFile "estoque.csv" AppendMode
    hPutStrLn arq (show id ++ "," ++ nome ++ "," ++ show preco ++ "," ++ marca ++ "," ++ show qtd)
    hClose arq

getProduto :: IO()
getProduto =
    do 
        arq <- openFile "teste.txt" ReadMode
        content <- hGetContents arq
        let partes = splitBy ',' content
        mapM_ putStrLn partes
        hClose arq


-- Função para remover um produto do arquivo de estoque
removerProduto :: Int -> String -> IO ()
removerProduto idProduto caminho = do
    withFile caminho ReadMode (\arquivo -> do
        conteudo <- hGetContents arquivo
        let linhas = lines conteudo
        let novasLinhas = filter2 (\linha -> not (show idProduto `isPrefixOf` linha))linhas
        -- let novasLinhas = filter2 (\linha -> not (isPrefixOf2 (show idProduto) (splitBy ',' linha))) linhas
        
        -- essa parte é responsável por forçar a avaliação completa do arquivo
        -- Se usar somente o hclose arquivo ele dá um erro informando que a operação é ilegal
        length conteudo `seq` return () 
        
        -- Sobrescrever o arquivo com as linhas restantes
        writeFile caminho (unlines novasLinhas)
        putStrLn $ "Produto com id " ++ show idProduto ++ " removido do estoque."
        )


-- Atualiza Produto
type Colunas = [String]
type Valores = [String]

percorreLinhas :: [String] -> String -> [String]
percorreLinhas [] comparador = []
percorreLinhas (l:ls) comparador
    | comparador == head (splitBy ',' l) = "novas":percorreLinhas ls comparador
    | otherwise = l:percorreLinhas ls comparador

atualizaProduto :: Int -> String -> Colunas -> Valores -> IO ()
atualizaProduto idProduto caminho col val = do
    withFile caminho ReadMode (\arquivo -> do
        conteudo <- hGetContents arquivo
        let linhas = lines conteudo
        let novasLinhas = percorreLinhas linhas (show idProduto)
        putStrLn ("\n\n" ++ show novasLinhas ++ "\n\n")
        -- let novasLinhas = filter2 (\linha -> not (isPrefixOf2 (show idProduto) (splitBy ',' linha))) linhas
        
        -- essa parte é responsável por forçar a avaliação completa do arquivo
        -- Se usar somente o hclose arquivo ele dá um erro informando que a operação é ilegal
        length conteudo `seq` return () 
        
        -- Sobrescrever o arquivo com as linhas restantes
        writeFile caminho (unlines novasLinhas)
        putStrLn $ "Produto com id " ++ show idProduto ++ " removido do estoque."
        )