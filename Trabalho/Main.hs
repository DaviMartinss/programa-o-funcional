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

-- #############################################################################################