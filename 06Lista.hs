main :: IO ()
main = do
    putStrLn "Digite um inteiro de 0 a 20:"
    pos <- readLn
    let lista = gerarLista pos

    putStrLn ("Sequencia de fibonacci ate a posicao " ++ show pos)
    exibirSequencia lista  

    let soma = somaSequencia lista
    putStrLn ("Soma total:" ++ show soma)
  

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n -1) + fibonacci (n-2)

gerarLista :: Int -> [Int]
gerarLista n = [fibonacci x | x <- [0 .. n]]

exibirSequencia :: [Int] -> IO ()
exibirSequencia [] = putStrLn ""
exibirSequencia (a:as) = do
    putStr (show a)
    putStr " "
    exibirSequencia as

somaSequencia :: [Int] -> Int
somaSequencia [] = 0
somaSequencia (a:as) = a + somaSequencia as