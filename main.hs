module Main where
import Produto(Produto(..))
import System.Process (system)
import Control.Concurrent (threadDelay)
import Utils
import Estoque (atualizarProduto, registrarProduto, criarEstoque, listaProdutos, removerProduto)

opcoes :: String
opcoes = "1 - Criar Estoque\n2 - Adicionar Produto\n3 - Atualizar Produto\n4 - Remover Produto\n0 - Sair\n"

coletaDadosProd :: IO()
coletaDadosProd = do
    id <- idAutoIncrement "estoque.csv"
    putStr "Digite o nome: "
    nome <- getLine
    putStr "Digite o preco: "
    preco <- getLine
    putStr "Digite a marca: "
    marca <- getLine
    putStr "Digite a quantidade: "
    qtd <- getLine
    registrarProduto (Produto id nome (read preco :: Float) marca (read qtd :: Int))

menu :: Int -> IO()
menu op = case op of
    1 -> do
        insereAtbs "estoque.csv" "id,nome,preco,marca,qtd\n"
        criarEstoque listaProdutos
        repeatMenu
    2 -> do
        putStrLn "Registrar produto no estoque"
        coletaDadosProd
        repeatMenu
    3 -> do
        putStrLn "Atualizar produto no estoque"
        putStr "Digite o id: "
        id <- getLine
        putStr "Digite o novo nome: "
        nome <- getLine
        putStr "Digite o novo preco: "
        preco <- getLine
        putStr "Digite a nova marca: "
        marca <- getLine
        putStr "Digite a nova quantidade: "
        qtd <- getLine
        atualizarProduto (read id :: Int) "estoque.csv" (Produto (read id :: Int) nome (read preco :: Float) marca (read qtd :: Int))
        repeatMenu
    4 -> do
        putStrLn "Remover produto do estoque"
        putStrLn "Digite o id do produto: "
        id <- getLine
        removerProduto (read id :: Int) "estoque.csv"
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