module Pedido where
import Produto (Produto(..))
import Item (Item(..))

-- Declaração de atributos 
type Total = Float
data Pedido = Pedido {
    items :: [Item],
    vlTotal :: Total
}

-- função responsavel por calcular o valor total de uma lista de items
calcTotal :: [Item] -> Float
calcTotal [] = 0.0
calcTotal ((Item (Produto _ _ preco _) quant):is) = fromIntegral quant * preco + calcTotal is

-- Esta função recebe um item e retona um pedido
realizaPedido :: [Item] -> Pedido
realizaPedido [] = Pedido [] 0
realizaPedido lista = Pedido lista (calcTotal lista)

-- Gera nota fiscal