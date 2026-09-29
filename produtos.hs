--Lista de produtos 
main :: IO ()
main = do
    putStrLn "Lista de produtos!"
    lista <- cadastroProdutos []
    putStrLn "Lista completa:"
    exibirProdutos lista
    putStrLn "Valor total:"
    let valor = valorTotal lista
    print valor
    putStrLn "Maior preco da lista:"
    let maior = maiorPreco lista
    print maior

cadastroProdutos :: [(String, Float)] -> IO [(String, Float)]
cadastroProdutos lista = do
    putStrLn "Digite o nome do produto:"
    nome <- getLine
    putStrLn "Digite o valor do produto:"
    valorProduto <- getLine
    let valor = read valorProduto :: Float
        novaLista = lista ++ [(nome, valor)]

    putStrLn "Deseja adicionar mais algum produto? (s/n)"
    opcao <- getLine 

    if opcao == "s" then cadastroProdutos novaLista else return novaLista

exibirProdutos :: [(String, Float)] -> IO ()
exibirProdutos [] = return ()
exibirProdutos ((nome, valor):as) = do
    putStrLn (nome ++ ", " ++ show valor)
    exibirProdutos as

valorTotal :: [(String, Float)]  -> Float
valorTotal [] = 0
valorTotal ((_, valor):as) = valor + valorTotal as 

maiorPreco :: [(String, Float)] -> Float
maiorPreco [] = 0
maiorPreco ((_, valor):as)
    | valor >= maiorPreco as = valor
    | otherwise = maiorPreco as 
