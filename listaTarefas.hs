main :: IO ()
main = menu []
    


menu :: [(String, String)] -> IO ()
menu lista = do
    putStrLn "Escolha uma opcao:"
    putStrLn "1. Adicionar Tarefa"
    putStrLn "2. Listar tarefas por categoria"
    putStrLn "3. Sair"
    opcao <- getLine

    if opcao ==  "1" then do
        putStrLn "Digite a tarefa:"
        tarefa <- getLine
        putStrLn "Digite a categoria:"
        categoria <- getLine

        let novaLista = [(tarefa, categoria)] ++ lista

        menu novaLista

    else if opcao == "2" then do
        putStrLn "Digite qual categoria voce deseja ver as tarefas correspondentes:"
        categ <- getLine

        let resultado = filter(\(_, cat) -> cat == categ) lista
        putStrLn ("Tarefas da categ " ++ categ ++ ":")
        listar resultado
        
        menu lista

    else if opcao == "3" then do
        putStrLn "Programa encerrado!"

    else do
        putStrLn "Opcao invalida, tente novamente!"
        menu lista

        

listar :: [(String, String)] -> IO ()
listar [] = return ()
listar ((t, _): as) = do
    putStrLn t
    listar as

