
main :: IO ()
main = do
    putStrLn "Lista de numeros!"
    lista <- adicionarNumeros []
    putStrLn ""
    putStrLn "Lista completa:"
    exibirLista lista
    putStrLn "Soma total dos numeros da lista:"
    let soma = calcularSoma lista
    print soma
    putStrLn "Media da lista:"
    let media = calcularMedia lista
    print media
    putStrLn "Maior numero da lista:"
    let maior = maiorNumero lista
    print maior


adicionarNumeros :: [Int] -> IO [Int]
adicionarNumeros lista = do
    putStrLn "Digite um numero:"
    numero <- getLine
    let num = read numero :: Int
    putStrLn "Deseja adicionar mais algum numero? (s/n)"
    opcao <- getLine

    let listaNova = lista ++ [num]

    if opcao == "s" then adicionarNumeros listaNova else return listaNova

exibirLista :: [Int] -> IO ()
exibirLista [] = return ()
exibirLista (a:as) = do
    print a
    exibirLista as

calcularSoma :: [Int] -> Int
calcularSoma [] = 0
calcularSoma (a:as) = a + calcularSoma as

calcularMedia :: [Int] -> Float
calcularMedia lista =
    let tam = length lista
        soma = calcularSoma lista
    in  fromIntegral soma / fromIntegral tam

maiorNumero :: [Int] -> Int
maiorNumero [] = 0
maiorNumero (a:as)
    | a >= maiorNumero as = a
    | otherwise           = maiorNumero as