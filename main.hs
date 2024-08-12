module Main where
import Estoque
import Produto

main :: IO()
main = do
    putStrLn "Digite o id do produto"
    id <- getLine
    putStrLn (getNomeProduto listaProdutos (read id :: Int))
