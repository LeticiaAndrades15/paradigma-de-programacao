--exercicios da lista 01 de haskell

--01 
custo :: Float -> Float
custo x = x + (x*0.12) + (x*0.3)

--02
aprovacao :: Float -> Float -> Float -> Float -> String
aprovacao x y z a
    |(((x+y)/2)*0.8 + ((z+a)/2)*0.2) >= 6.0 = "Aprovado"
    |otherwise = "nao aprovado"

--03
ocorrencia :: String -> Char -> Int
ocorrencia [] x = 0
ocorrencia (a:as) x
    |a == x = ocorrencia as x + 1
    |otherwise = ocorrencia as x

--04
palindromo :: String -> String
palindromo word
    |word == reverse word = "Sim"
    |otherwise = "Nao"

--05 
main :: IO ()
main = do
    putStrLn "Lista de gastos!"
    lista <- adicionarItem []
    putStrLn "Total de gastos:"
    let total = exibirTotal lista
    print total

adicionarItem :: [(String, Float)] -> IO [(String, Float)]
adicionarItem lista = do
    putStrLn "Digite o item:"
    item <- getLine
    putStrLn "Digite o valor:"
    valor <- readLn

    let novaLista = (item, valor) : lista

    putStrLn "Deseja adicionar mais um item? (s/n)"
    opc <- getLine
    if opc == "s"
        then adicionarItem novaLista
        else return novaLista

exibirTotal :: [(String, Float)] -> Float
exibirTotal [] = 0
exibirTotal ((_, valor):as) = valor + exibirTotal as


