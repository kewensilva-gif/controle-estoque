module Produto where

-- Declaração de atributos
type Id = Int
type Nome = String
type Preco = Float
type Marca = String
type Quant = Int
data Produto = Produto Id Nome Preco Marca Quant