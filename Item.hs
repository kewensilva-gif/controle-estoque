module Item where
import Produto (Produto)

-- Declaração de atributos 
type Quant = Int
data Item = Item Produto Quant