-- Q1 ##############################################################################################
import Data.Char (isDigit) -- importando a função isDigit do módulo Data.Char

getDigito :: Char -> Int
getDigito c = read [c]

-- Função que adiciona a soma dos dígitos ao final de uma string de oito dígitos
addSum :: String -> String
addSum s = s ++ verificaSoma
    where
        somaOitoDigitos = sum (map getDigito s)
        verificaSoma = if somaOitoDigitos < 10 then 
            "0" ++ show somaOitoDigitos else show somaOitoDigitos

isDigitString :: String -> Bool -- definindo o tipo da função isDigitString
isDigitString s = all isDigit s --usando a função all para aplicar isDigit em todos os caracteres da string s

-- Q2 #############################################################################################
validaCartao :: String -> Bool
validaCartao numCartao
    | not (isDigitString numCartao) = False -- Verifica se todos os digitos são numéricos
    | (length numCartao) /= 10 = False -- Verifica a quantidade de dígitos para validar
    | otherwise = somaOitoDigitos == resultadoVerificadores
        where
            (significativos, verificadores) = splitAt 8 numCartao -- Dividi os significativos e verificadores
            somaOitoDigitos = sum (map getDigito significativos) -- Somando os significativos
            resultadoVerificadores = read verificadores :: Int -- Convertendo o valor da string para um valor inteiro

-- Q3 #############################################################################################

-- função que verifica se o caractere é uma vogal
isVogal :: Char -> Bool
isVogal s = elem s "aeiou" || elem s "AEIOU" -- pode ter vogais maiúsculas? Acho que sim.

-- função que verifica se o caractere é uma consoante
isConsoante :: Char -> Bool
isConsoante s = (elem s ['A'..'Z'] || elem s ['a'..'z']) && not (isVogal s)

--função que conta a quantidade de vogais
contaVogais :: String -> Int 
contaVogais s = length $ filter isVogal s

--função que conta a quantidade de consoantes
contaConsoantes :: String -> Int 
contaConsoantes s = length $ filter isConsoante s

--função que verifica se a String tem de 4 à 8 caracteres
validaCaracteres :: String -> Bool
validaCaracteres s = and [length s >= 4, length s <= 8]

-- função que valida a senha de acesso
-- Somente ou pelo menos duas vogais e consoantes ?
validaSenha :: String -> Bool 
validaSenha s = validaCaracteres s && contaVogais s >= 2 && contaConsoantes s >= 2 

-- Q4 #############################################################################################
verificaListaSenhas :: [String] -> [String]
verificaListaSenhas [] = []
verificaListaSenhas lista = filter validaSenha lista


-- Função Main ##############################################################################
main :: IO ()
main = do
    putStrLn "Digite um identificador de cartão:"
    s <- getLine
    if validaCartao s
        then putStrLn ("O identificador de cartão " ++ s ++ " é válido!")
        else putStrLn "O identificador de cartão é inválido!"