module Estoque where
import Produto ( Produto(..) )
import Item ( Item(..) )
import GHC.List
import System.IO

-- Declaração de atributos
type Quant = Int
type IdProd = Int
type Estoque = [Item]
listaProdutos :: Estoque
listaProdutos = [ 
        Item (Produto 0 "Queijo" 1.4 "Natville") 5,
        Item (Produto 1 "Leite" 2.0 "Italac") 10,
        Item (Produto 2 "Pão" 0.5 "Panificadora") 15,
        Item (Produto 3 "Café" 3.2 "Melitta") 8,
        Item (Produto 4 "Açúcar" 1.8 "União") 12,
        Item (Produto 5 "Manteiga" 2.5 "Aviação") 7,
        Item (Produto 6 "Arroz" 4.0 "Tio João") 20,
        Item (Produto 7 "Feijão" 3.0 "Camil") 18,
        Item (Produto 8 "Macarrão" 2.3 "Galo") 9,
        Item (Produto 9 "Óleo" 3.5 "Liza") 6
    ]

-- Métodos
getNomeProduto :: Estoque -> IdProd -> String
getNomeProduto [] _ = "Produto Inexistente"
getNomeProduto (Item (Produto id nome _ _) _:ps) idSearch
    | id == idSearch = nome
    | otherwise = getNomeProduto ps idSearch

getPrecoProduto :: Estoque -> IdProd -> Float
getPrecoProduto [] _ = 0.0
getPrecoProduto (Item (Produto id _ preco _) _:ps) idSearch
    | id == idSearch = preco
    | otherwise = getPrecoProduto ps idSearch

getMarcaProduto :: Estoque -> IdProd -> String
getMarcaProduto [] _ = "Produto Inexistente"
getMarcaProduto (Item (Produto id _ _ marca) _:ps) idSearch
    | id == idSearch = marca
    | otherwise = getMarcaProduto ps idSearch

-- adiciona produto ao estoque
registrarItem:: Item -> IO()
registrarItem (Item (Produto id nome preco marca) qtd)= 
    appendFile "teste.txt" (show id ++ "," ++ nome ++ "," ++ show preco ++ "," ++ marca ++ "," ++ show qtd ++ "\n")

splitBy :: Char -> String -> [String]
splitBy _ [] = [""]
splitBy delimiter (c:cs)
    | c == delimiter = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = splitBy delimiter cs

lerArqProduto :: IO()
lerArqProduto = 
    do 
        arq <- openFile "teste.txt" ReadMode
        content <- hGetContents arq
        let partes = splitBy ',' content
        mapM_ putStrLn partes
        hClose arq


-- remove produto do estoque