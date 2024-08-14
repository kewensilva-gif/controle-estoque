module Main where
import Estoque
import Produto

opcoes :: String
opcoes = "1 - Retorna nome\n2 - Retorna Quantidade\n0 - Sair"

menu :: Int -> IO()
menu op = case op of
    1 -> do
        putStrLn "Digite o id do produto"
        id <- getLine
        putStrLn (getNomeProduto listaProdutos (read id :: Int))
        repeatMenu
    3 -> do
        putStrLn "Registrar item no estoque:"
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
        registrarItem (Produto (read id :: Int) nome (read preco :: Float) marca (read qtd :: Int))
        

    0 ->
        do
            putStrLn "Encerra"

repeatMenu :: IO()
repeatMenu = do
    putStr opcoes
    putStr "Digite a opção desejada: "
    op <- getLine
    menu (read op :: Int)


main :: IO()
main = do
    repeatMenu