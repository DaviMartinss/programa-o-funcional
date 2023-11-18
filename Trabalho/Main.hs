-- Q1 #######################################################################################
import Data.Char (isDigit) -- importando a função isDigit do módulo Data.Char

getDigito :: Char -> Int
getDigito c = read [c]

-- Função que adiciona a soma dos dígitos ao final de uma string de oito dígitos
addSum :: String -> String
addSum s = s ++ show (sum (map getDigito s))

isDigitString :: String -> Bool -- definindo o tipo da função isDigitString
isDigitString s = all isDigit s --usando a função all para aplicar isDigit em todos os caracteres da string s

-- Função principal que recebe a entrada do teclado e chama a função addSum
main :: IO ()
main = do
    putStrLn "Digite uma string de oito dígitos:"
    s <- getLine
    if length s == 8 && isDigitString s
        then putStrLn ("O resultado é: " ++ addSum s)
        else putStrLn "Erro: entrada inválida."

-- Q3 #############################################################################################

-- função que verifica se o caractere é uma vogal
isVogal :: Char -> Bool
isVogal s = elem s "aeiou" -- pode ter vogais maiúsculas?

-- função que verifica se o caractere é uma consoante
isConsoante :: Char -> Bool
isConsoante s = elem s ['A'..'Z']

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
validaSenha :: String -> Bool
validaSenha s = validaCaracteres s && contaVogais s == 2 && contaConsoantes s == 2