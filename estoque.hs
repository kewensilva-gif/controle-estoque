module Estoque where
import Produto ( Produto(..) )
import Item ( Item(..) )

-- Declaração de atributos
type Quant = Int
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