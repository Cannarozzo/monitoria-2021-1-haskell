data Metros = Metros{dimensao::Int,valor::Double} | Invalido
    deriving Show

{-
data Metros2 = Metros2 Int Double deriving Show

dimensaoNaMao :: Metros2 -> Int
dimensaoNaMao (Metros2 d _ ) = d

valorNaMao :: Metros2 -> Double
valorNaMao (Metros2 _ v) = v
-}

-- to implement with type families
areaQuadrado :: Metros -> Metros
areaQuadrado (Metros dimensao tamanho) = Metros 2 (tamanho ^ 2)

areaCubica :: Metros -> Metros
areaCubica (Metros 1 tamanho ) = Metros 3 (tamanho ^ 3)
areaCubica (Metros 2 tamanho ) = Metros 3 (tamanho * 2)
areaCubica (Metros _ _) = Invalido
areaCubica Invalido = Invalido

-- funcao parcial



data Bateria = Bateria{marcaB::String} deriving Show
data Tela = Tela{marcaT::String} deriving Show
data Hardware = Hardware {marcaH::String} deriving Show

data Celular = Celular Hardware Tela Bateria deriving Show



removerTelaCelular :: Celular -> Tela
removerTelaCelular (Celular h t b) = t

adicionarNovaTela :: Celular -> Tela -> Celular
adicionarNovaTela (Celular h telaAntiga b) telaNova = Celular h telaNova b 

--
mediaLista :: [Double] -> Double
mediaLista xs = foldl (+) 0 xs / fromIntegral (length xs)

