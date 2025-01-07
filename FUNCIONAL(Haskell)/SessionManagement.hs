module SessionManagement where

import Data.IORef
import Control.Monad (when)
import System.IO
import Data.Time (getCurrentTime, utctDay)

-- Definição dos tipos de usuário e sessão
data UserType = Employee | Customer deriving (Eq, Show)
data Session = Session 
    { movieTitle :: String
    , time :: String
    , room :: String
    , date :: String
    , ticketPrice :: Double
    , audioType :: String  -- Novo campo para tipo de áudio (Legendado/Dublado)
    } deriving (Show, Read)


-- Função para verificar se o usuário é funcionário
isEmployee :: UserType -> Bool
isEmployee Employee = True
isEmployee _        = False

-- Função para salvar sessões em um arquivo (sobrescrevendo o arquivo)
saveSessionsToFile :: [Session] -> IO ()
saveSessionsToFile sessions = do
    withFile "sessions.txt" WriteMode $ \handle -> do
        hPutStrLn handle (show sessions)

-- Função para carregar sessões de um arquivo
loadSessionsFromFile :: IO [Session]
loadSessionsFromFile = do
    contents <- readFile "sessions.txt"
    return (if null contents then [] else read contents)

-- Função para criar uma nova sessão de cinema
addSession :: UserType -> IORef [Session] -> IO ()
addSession user sessionsRef = 
    when (isEmployee user) $ do
        addMultipleSessions sessionsRef
        putStrLn "Sessões adicionadas com sucesso."

-- Função auxiliar para adicionar múltiplas sessões
addMultipleSessions :: IORef [Session] -> IO ()
addMultipleSessions sessionsRef = do
    putStrLn "Adicionar nova sessão: Informe o título do filme."
    title <- getLine
    putStrLn "Informe o horário da sessão (por exemplo, 15:00)."
    sessionTime <- getLine
    putStrLn "Informe a sala (por exemplo, Sala 3)."
    sessionRoom <- getLine
    putStrLn "Informe a data de exibição do filme no formato DD/MM (por exemplo, 25/08)."
    sessionDate <- getLine
    putStrLn "Informe o preço do ingresso (por exemplo, 20.50)."
    price <- readLn :: IO Double
    putStrLn "Informe o tipo de áudio (Legendado ou Dublado)."
    audio <- getLine

    let newSession = Session 
            { movieTitle = title
            , time = sessionTime
            , room = sessionRoom
            , date = sessionDate
            , ticketPrice = price
            , audioType = audio  -- Inclui o tipo de áudio na nova sessão
            }
    modifyIORef sessionsRef (\sessions -> newSession : sessions)

    -- Atualiza o arquivo sessions.txt com a nova sessão
    updatedSessions <- readIORef sessionsRef
    saveSessionsToFile updatedSessions


-- Função para editar uma sessão de cinema
editSession :: UserType -> IORef [Session] -> IO ()
editSession user sessionsRef = 
    if isEmployee(user)
    then do
        putStrLn "Editar sessão: Informe o título do filme da sessão que deseja editar."
        title <- getLine
        putStrLn "Informe o novo horário da sessão (por exemplo, 17:00)."
        newTime <- getLine
        putStrLn "Informe a nova sala (por exemplo, Sala 4)."
        newRoom <- getLine
        putStrLn "Informe a nova data de exibição do filme (por exemplo, 2024-08-25)."
        newDate <- getLine
        putStrLn "Informe o novo preço do ingresso (por exemplo, 25.00)."
        newPrice <- readLn :: IO Double
        putStrLn "Informe o novo tipo de áudio (Legendado ou Dublado)."
        newAudio <- getLine

        -- Atualizar a sessão na lista
        modifyIORef sessionsRef (map (\s -> if movieTitle s == title 
                                            then s { time = newTime
                                                   , room = newRoom
                                                   , date = newDate
                                                   , ticketPrice = newPrice
                                                   , audioType = newAudio }  -- Atualiza o tipo de áudio
                                            else s))
        
        -- Salvar as sessões atualizadas no arquivo
        sessions <- readIORef sessionsRef
        saveSessionsToFile sessions

        putStrLn "Sessão editada com sucesso."
    else
        putStrLn "Apenas funcionários podem editar sessões."


-- Função para remover uma sessão de cinema
removeSession :: UserType -> IORef [Session] -> IO ()
removeSession user sessionsRef = 
    if isEmployee user
    then do
        putStrLn "Remover sessão: Informe o título do filme da sessão que deseja remover."
        title <- getLine

        -- Remover a sessão da lista
        modifyIORef sessionsRef (filter (\s -> movieTitle s /= title))
        
        -- Salvar as sessões atualizadas no arquivo
        sessions <- readIORef sessionsRef
        saveSessionsToFile sessions

        putStrLn "Sessão removida com sucesso."
    else
        putStrLn "Apenas funcionários podem remover sessões."

-- Função para exibir todas as sessões para usuários-clientes
viewSessions :: IORef [Session] -> IO ()
viewSessions sessionsRef = do
    sessions <- readIORef sessionsRef
    if null sessions
    then putStrLn "Nenhuma sessão disponível."
    else mapM_ printSessionDetails sessions

-- Função auxiliar para imprimir os detalhes de uma sessão
printSessionDetails :: Session -> IO ()
printSessionDetails (Session title time room date price audio) = do
    putStrLn $ "Filme: " ++ title
    putStrLn $ "Horário: " ++ time
    putStrLn $ "Sala: " ++ room
    putStrLn $ "Data: " ++ date
    putStrLn $ "Preço do Ingresso: R$ " ++ show price
    putStrLn $ "Tipo de Áudio: " ++ audio  -- Exibe o tipo de áudio (Legendado/Dublado)
    putStrLn "------------------------"


-- Função principal de gerenciamento de sessões
manageSessions :: UserType -> IORef [Session] -> IO ()
manageSessions user sessionsRef = do
    putStrLn "1) Criar Sessão"
    putStrLn "2) Editar Sessão"
    putStrLn "3) Remover Sessão"
    putStrLn "4) Visualizar Sessões"
    putStrLn "5) Voltar ao Menu Principal"
    option <- getLine
    case option of
        "1" -> addSession user sessionsRef >> manageSessions user sessionsRef
        "2" -> editSession user sessionsRef >> manageSessions user sessionsRef
        "3" -> removeSession user sessionsRef >> manageSessions user sessionsRef
        "4" -> viewSessions sessionsRef >> manageSessions user sessionsRef
        "5" -> putStrLn "Voltando ao menu principal..."
        _   -> putStrLn "Opção inválida. Tente novamente." >> manageSessions user sessionsRef

-- Renomeando a função main para evitar conflitos
sessionManagementMain :: IO ()
sessionManagementMain = do
    sessions <- loadSessionsFromFile
    sessionsRef <- newIORef sessions
    putStrLn "Informe o tipo de usuário (Employee ou Customer):"
    userTypeInput <- getLine
    let userType = if userTypeInput == "Employee" then Employee else Customer
    manageSessions userType sessionsRef