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