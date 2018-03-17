module Questao2 where

import qualified Data.Set as S

-- Verificador de tautologia --

-- | define precedencia dos operadores
infixl 2 :&:
infixl 4 :->:

-- | Tipo algebrico que representa uma proposição
data Prop = Prop :->: Prop | Prop :&: Prop | Not Prop | Var Char
-- | Tipo que representa uma tabela que associa variaveis as suas valoracoes
type Subs = [(Char, Bool)]

-- | operador infixo que procura uma valor em um Subs (semelhante a lookup)
infix //
(//) :: Subs -> Prop -> Bool
(//) [] _ = False
(//) (x:xs) (Var var)
    | fst x == var = snd x
    | otherwise = xs // (Var var)

-- | avalia o valor de uma proposicao
eval :: Subs -> Prop -> Bool
eval subs prop = case prop of
    v@(Var _)  -> subs // v -- retorna valor de v de acordo com subs
    Not p      -> not $ eval subs p
    p1 :&: p2  -> (&&) (eval subs p1) (eval subs p2)
    p1 :->: p2 -> implic (eval subs p1) (eval subs p2)
    where
        implic True False = False
        implic _    _     = True  


-- | retorna o conjunto das variaveis de uma proposicao
vars :: Prop -> S.Set Char
vars (Var v) = S.singleton v
vars (p1 :->: p2) = S.union (vars p1) (vars p2)
vars (p1 :&: p2) = S.union (vars p1) (vars p2)
vars (Not p) = S.union (vars p) S.empty

-- | retorna uma lista com todas as valoracoes de tamanho n
toBin 0 = [0]
toBin n | n `mod` 2 == 1 = toBin (n `div` 2) ++ [1]
        | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]

-- | redimensiona um binario para um array de tamanho n
resizeBin :: Int -> [Int] -> [Int] 
resizeBin n bin 
                | len == n  = bin
                | len > n   = dropWhile (== 0) bin
                | otherwise = bin ++ (replicate (n - len) 0)
                where
                    len = length bin

-- | bools :: Int -> [[Bool]]
bools n = map (map intToBool . resizeBin n . toBin) [x | x <- [0.. (2 ^ n - 1)]]
        where
            intToBool i = if i == 0 then False else True

-- | retorna todas as possibilidades de valoracao das variaveis de uma Prop
substs :: Prop -> [Subs]
substs p = map (zip vs) bs
        where
            vs = S.toList . vars $ p
            bs = bools $ length vs

-- | avalia se uma Prop é tautologia
-- isTaut :: Prop -> Bool
isTaut prop = foldr (&&) True $ map (\sub -> eval sub prop) subsList
            where
                subsList = substs prop