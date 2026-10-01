main :: IO ()
main = do
    putStrLn "Informe o valor de x:"
    x <- readLn
    let multiplos = [n | n <- [1..100], n `mod` x == 0]
    print multiplos