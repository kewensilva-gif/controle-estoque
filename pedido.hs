module Pedido where
import Produto (Produto)
import Item (Item)

-- Declaração de atributos 
type Total = Float
data Pedido = Pedido [Item] Total
