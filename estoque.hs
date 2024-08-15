module Estoque where
import Produto ( Produto(..) )
-- import GHC.List
import System.IO
import Utils (filter2, splitBy, filterMap, converterProdutoEmString)
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

-- adiciona produto ao estoque
registrarProduto:: Produto -> IO()
registrarProduto (Produto id nome preco marca qtd) = do
    arq <- openFile "estoque.csv" AppendMode
    hPutStrLn arq (show id ++ "," ++ nome ++ "," ++ show preco ++ "," ++ marca ++ "," ++ show qtd)
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

modificarElemento :: [String] -> String -> String -> [String]
modificarElemento  [] _ _ = []
modificarElemento  (l:ls) comparador novo
    | comparador `isPrefixOf` l = novo:modificarElemento ls comparador novo
    | otherwise = l:modificarElemento ls comparador novo

atualizarProduto :: Int -> String -> Produto -> IO ()
atualizarProduto idProduto caminho produto = do
    withFile caminho ReadMode (\arquivo -> do
        conteudo <- hGetContents arquivo
        let linhas = lines conteudo
        
        let novasLinhas = modificarElemento linhas (show idProduto) (converterProdutoEmString produto)
        -- let novasLinhas = filter2 (\linha -> not (isPrefixOf2 (show idProduto) (splitBy ',' linha))) linhas
        
        -- essa parte é responsável por forçar a avaliação completa do arquivo
        -- Se usar somente o hclose arquivo ele dá um erro informando que a operação é ilegal
        length conteudo `seq` return () 
        
        -- Sobrescrever o arquivo com as linhas restantes
        writeFile caminho (unlines novasLinhas)
        putStrLn $ "Produto com id " ++ show idProduto ++ " atualizado no estoque."
        )