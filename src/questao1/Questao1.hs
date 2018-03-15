module Questao1 where

import Data.List (group, sortBy)

-- | transforma char [a-z] em inteiro [0-25]
charToInt :: Char -> Int
charToInt c = sum . map (\x -> 1) . takeWhile (\ch -> ch /= c) $ ['a' .. 'z']

-- | transforma inteiro [0-25] em char [a-z]
intToChar :: Int -> Char
intToChar i = ['a' .. 'z'] !! (i `mod` 26)

-- | codifica string para Cifra de César
encode :: Int -> String -> String
encode n l = map (\c -> if c == ' ' then c else f c) l
            where
              f = intToChar . (+) n . charToInt

-- | freq letras da língua inglesa
expectedFreq = [8.167, 1.492, 2.782, 4.253, 12.702, 2.228, 2.015, 6.094, 6.966, 0.153, 0.772, 4.025, 2.406, 6.749, 7.507, 1.929, 0.095, 5.987, 6.327, 9.056, 2.758, 0.978, 2.360, 0.150, 1.974, 0.074]  :: [Float]

-- | calcular porcentagem de a/b
percent :: Int -> Int -> Float
percent a b = fromIntegral a / fromIntegral b * 100

-- | transforma string em uma lista com a frequência de seus caracters
-- | lista de saída sempre tem tamanho 26
freqs :: String -> [Float]
freqs str = map (\x -> percent x len) . map snd $ freq
          where
            len = length . filter (/= ' ') $ str
            freq = [(x, count) | x <- ['a' .. 'z'], let count = (length . filter (==x)) str]

-- | algoritmo qui-quadrado
chisqr :: [Float] -> [Float] -> Float
chisqr obs exp = sum . zipWith (\o e -> (o - e) ^ 2 / e) obs $ exp

-- | rotaciona uma lista por um fator n
rotate :: Int -> [a] -> [a]
rotate n l =  drop n l ++ take n l

-- | decodifica uma string
decode :: Int -> String -> String
decode n str = encode (-n) str

-- | quabra um string codificada na Cifra de César
crack :: String -> String
crack str =  (decode . fst . head . sortBy sortF . zip [0..25] $ chi) str
          where
            table = freqs str
            chi = [chisqr (rotate n table) expectedFreq | n <- [0..25]]
            sortF (_, a) (_, b) = if a >= b then GT else LT