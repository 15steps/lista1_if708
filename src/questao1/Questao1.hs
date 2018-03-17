module Questao1 where

import Data.List (group, sortBy)

data Language = PT | EN

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
freqsEn = [8.167, 1.492, 2.782, 4.253, 12.702, 2.228, 2.015, 6.094, 6.966, 0.153, 0.772, 4.025, 2.406, 6.749, 7.507, 1.929, 0.095, 5.987, 6.327, 9.056, 2.758, 0.978, 2.360, 0.150, 1.974, 0.074]  :: [Float]

-- | freq letras da língua portuguesa
freqsPt = [14.63, 1.04,3.88,4.99,12.57,1.02,1.3,1.28,6.18,0.4,0.02,2.78,4.74,5.05,10.73,2.52,1.2,6.53,7.81,4.34,4.63,1.67,0.01,0.21,0.01,0.47] :: [Float]

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
crack str = (decode . fst . head . sortBy sortF . zip [0..25] $ chi) str
          where
            table = freqs str
            chi = [chisqr (rotate n table) freqsEn | n <- [0..25]]
            sortF (_, a) (_, b) = if a >= b then GT else LT

crackAux :: String -> [Float] -> String
crackAux str f = (decode . fst . head . sortBy sortF . zip [0..25] $ chi) str
                where
                  table = freqs str
                  chi = [chisqr (rotate n table) f | n <- [0..25]]
                  sortF (_, a) (_, b) = if a >= b then GT else LT

crack2 :: String -> Language -> String
crack2 str lang = case lang of
                EN -> crackAux str freqsEn
                PT -> crackAux str freqsPt