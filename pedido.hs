module Pedido where
import Produto (Produto(..))

-- Declaração de atributos 
type Total = Float
data Pedido = Pedido {
    items :: [Produto],
    vlTotal :: Total
}

-- função responsavel por calcular o valor total de uma lista de items
calcTotal :: [Produto] -> Float
calcTotal [] = 0.0
calcTotal ((Produto _ _ preco _ quant):is) = fromIntegral quant * preco + calcTotal is

-- Esta função recebe um item e retona um pedido
realizaPedido :: [Produto] -> Pedido
realizaPedido [] = Pedido [] 0
realizaPedido lista = Pedido lista (calcTotal lista)

-- Gera nota fiscal