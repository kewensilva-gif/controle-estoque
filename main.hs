module Main where
import Estoque
import Produto
import System.Process (system)
import Control.Concurrent (threadDelay)
import Utils

opcoes :: String
opcoes = "1 - Criar Estoque\n2 - Adicionar Produto\n0 - Sair\n"

coletaDadosProd :: IO()
coletaDadosProd = do
    putStrLn "Digite o id: "
    id <- getLine
    putStrLn "Digite o nome: "
    nome <- getLine
    putStrLn "Digite o preco: "
    preco <- getLine
    putStrLn "Digite a marca: "
    marca <- getLine
    putStrLn "Digite a quantidade: "
    qtd <- getLine
    registrarProduto (Produto (read id :: Int) nome (read preco :: Float) marca (read qtd :: Int))

menu :: Int -> IO()
menu op = case op of
    1 -> do
        insereAtbs "estoque.csv" "id,nome,preco,marca,qtd\n"
        criarEstoque listaProdutos
        repeatMenu
    2 -> do
        putStrLn "Registrar produto no estoque:"
        coletaDadosProd
        repeatMenu

    0 ->
        do
            putStrLn "Encerra"

repeatMenu :: IO()
repeatMenu = do
    threadDelay (2 * 1000000)
    _ <- system "cls"
    putStr opcoes
    putStr "Digite a opção desejada: "
    op <- getLine
    menu (read op :: Int)


main :: IO()
main = do
    repeatMenu