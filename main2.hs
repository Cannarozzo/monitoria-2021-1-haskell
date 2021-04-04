{-
1. (valor 2.5 pontos)
Considere o tipo data Tarefa = Adicionar String | Remover deriving Show
(a) (0.25) Qual o valor da express˜ao :t Adicionar;
(b) (0.25) Qual o valor da express˜ao :t Remover;
(c) (0.5) Implemente uma fun¸c˜ao que retire todas as ocorrˆencias de Remover de uma lista de tarefas;
(d) (0.5) Implemente uma fun¸c˜ao que retorna a quantidade de valores Remover de uma lista de tarefas;
(e) (1.0) Implemente a fun¸c˜ao executar :: [Tarefa] -> [String] -> [String] que ao receber ocorrˆencias;
do valor Adicionar dever´a adicionar seu campo String `a lista de Strings do parˆametro; e ao receber
Remover, retire o primeiro elemento da lista de Strings.
Por exemplo, executar [Remover,Adicionar "FATEC",Remover,Adicionar ""HASKELL"] ["ETEC","SANTOS"]
retorna ["FATEC","HASKELL"].
-}

data Tarefa = Adicionar String | Remover deriving (Show,Eq)

--A :t Adicionar =  Adicionar :: String -> Tarefa

--b :t Remover = Remover :: Adicionar

--c 
removerRemove :: [Tarefa] -> [Tarefa]
removerRemove [] = []
removerRemove (x:xs)
    | x == Remover = removerRemove xs
    | otherwise = x : (removerRemove xs)
    
removerRemove' :: [Tarefa] -> [Tarefa]
removerRemove' [] = []
removerRemove' xs = filter (\ tarefa -> tarefa /= Remover) xs

listaTarefas = [Adicionar "T1", Remover ,Adicionar "T2", Remover]

--d
qtdRemoverListaTarefas :: [Tarefa] -> Int
qtdRemoverListaTarefas [] = 0
qtdRemoverListaTarefas (x:xs)
    | x == Remover = 1 + qtdRemoverListaTarefas xs
    | otherwise = qtdRemoverListaTarefas xs
    
qtdRemoverListaTarefas' :: [Tarefa] -> Int
qtdRemoverListaTarefas' xs = foldl (\ valorBase valorLista -> valorBase + 1) 0 $ filter (\ tarefa -> tarefa == Remover) xs

{-
2. (valor 2.5 pontos)
E poss´ıvel fazer o tipo ´ data K = K um mon´oide? Se sim, apresente a instˆancia e verifique se a opera¸c˜ao bin´aria
escolhida e seu neutro satisfazem os axiomas. Caso contr´ario, explique com clareza o motivo.
-}
data K = K deriving Show

instance Semigroup K where
    K <> K = K

instance Monoid K where
    mempty = K
--    x `mappend` x2  = K

{-
Resposta: Sim, pois:
Identity Laws (left and right):
K <> mempty = K
mempty <> k = k

Associativity Law
(K <> K) <> K = K <> (K <> k)
-} 

--3
{-
Considere a fun¸c˜ao e responda
(a) (1.0) Qual o valor da express˜ao wd [8,9,-7,3,-2];
(b) (1.0) Fa¸ca o ”teste de mesa” da express˜ao acima;
(c) (0.5) Explique com clareza a linha 3, 4 e 5.
-}
wd :: (Ord a) => [a] -> [a]
wd [] = []
wd (x:xs) = wd sm ++ [x] ++ wd ms
    where
       ms = filter (>= x) xs
       sm = filter (<= x) xs

--a [-7,-2,3,8,9]

--b
{-
                [8,9,-7,3,-2] 

            [-7,-2, 3] ++ [8] ++ [9]

        [-7] ++ [-2, 3]

            [] ++ [-2] ++ [3]
-}

--c 
{-
linha 3: A lista é decomposta e o elemento head é colocado no centro.
Os elementos restantes sao adicionados a direita e a esquerda do head.
linha 4: uma funcao auxiliar ms que serve para filtar todos os elementos maiores que o head e concatenar a lista resultando a direta de head
linha 5: o mesmo que a linha quatro,(funcao auxiliar sm) mas os elementos menores que head a esquerda de head.
-}

--4
{-
4. (valor 2.5 pontos)
Considere o tipo data Quatro a = Quatro a String String a
(a) (0.25) Qual o kind do tipo Quatro?
(b) (0.25) Qual o kind do tipo Quatro Bool?
(c) (0.25) Mostre um valor do tipo Quatro Char
(d) (0.25) O tipo Quatro K (vide exerc´ıcio 2) ´e polim´orfico? Explique.
(e) (0.5) Crie uma instˆancia de Functor para este tipo.
(f) (0.5) Calcule fmap reverse (Quatro "FATEC" "FATEC" "FATEC" "FATEC")
(g) (0.5) Implemente uma instˆancia de Show para o tipo acima exeibindo apenas o primeiro e o quatro campo
como resposta.
-}
data Quatro a = Quatro a String String a -- deriving Show

--a 
-- Quatro :: * -> * , kind 2

--b Quatro Bool :: * , kind 2

--c 
--Quatro 'c' "Teste" "Teste" 'a'

--d
--Tipo Quatro a é polimorfico, porém, quando o tipo K é definido e ele tem apenas um value constructor; assumindo apenas uma forma.
--Logo o tipo Quatro K não é polimorfico é monomorfico.

--e
instance Functor Quatro where
    fmap funcao (Quatro a s s2 a2) = Quatro (funcao a) s s2 (funcao a2)
    
--f
--Calcule fmap reverse (Quatro "FATEC" "FATEC" "FATEC" "FATEC")
--Quatro "CETAF" "FATEC" "FATEC" "CETAF"

--g 
instance (Show a) => Show (Quatro a) where
    show (Quatro a _ _ a2) = show a ++ " " ++ show a2
    
--5
{-

(valor 2.5 pontos)
Complete os espa¸cos com nomes (lado esquerdo) e com express˜oes (lado direito) que n˜ao sejam undefined e
nem produzam erros de compila¸c˜ao:
(a) f1 :: (a -> b -> b) -> a -> b -> b
f1 ___ x y = _________________
(b) f2 :: [a] -> (a -> b) -> [b]
f2 ___ = _________________
(c) f3 :: (a -> b) -> (a,c) -> (b,c)
f3 ___ (x,y) = _________________
(d) f4 :: (a,b,c,d,e,f,g) -> e
f4 _____________ = _________________
(e) f5 :: a -> (a,a,a,String)
f5 a = _________________

-}

-- (a) 
f1 :: (a -> b -> b) -> a -> b -> b
f1 funcao x y = funcao x y

--(b) 

f2 :: [a] -> (a -> b) -> [b]
f2 xs funcao = fmap funcao xs

f2' :: [a] -> (a -> b) -> [b]
f2' [] _ = []
f2' (x:xs) funcao = [funcao x] ++ f2' xs funcao


--(c) 
f3 :: (a -> b) -> (a,c) -> (b,c)
f3 funcao (x,y) = (funcao x, y) 
--(d) 
f4 :: (a,b,c,d,e,f,g) -> e
f4 (_,_,_,_,x,_,_) = x
--(e) 
f5 :: a -> (a,a,a,String)
f5 a = (a,a,a, "Text")
