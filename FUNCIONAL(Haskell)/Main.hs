module Main where

import MovieManagement
import SessionManagement
import UserManagement
import ConcessionStand
import ReviewManagement (calculateAverageRating) -- Importa a função para cálculo das médias
import ClientInterface (runClientMode) -- Importa a função runClientMode do módulo ClientInterface
import Data.IORef (newIORef, IORef, readIORef, writeIORef)

-- Função para exibir a mensagem de boas-vindas ao Cine Taperoá
showWelcomeMessage :: IO ()
showWelcomeMessage = do
  putStrLn"████████╗ █████╗ ██████╗ ███████╗██████╗  ██████╗  █████╗ "
  putStrLn"╚══██╔══╝██╔══██╗██╔══██╗██╔════╝██╔══██╗██╔═══██╗██╔══██╗"
  putStrLn"   ██║   ███████║██████╔╝█████╗  ██████╔╝██║   ██║███████║"
  putStrLn"   ██║   ██╔══██║██╔═══╝ ██╔══╝  ██╔══██╗██║   ██║██╔══██║"
  putStrLn"   ██║   ██║  ██║██║     ███████╗██║  ██║╚██████╔╝██║  ██║"
  putStrLn"   ╚═╝   ╚═╝  ╚═╝╚═╝     ╚══════╝╚═╝  ╚═╝ ╚═════╝ ╚═╝  ╚═╝"
  putStrLn"███████╗██╗██╗     ███╗   ███╗███████╗███████╗    ██╗     "
  putStrLn"██╔════╝██║██║     ████╗ ████║██╔════╝██╔════╝    ██║     "
  putStrLn"█████╗  ██║██║     ██╔████╔██║█████╗  ███████╗    ██║     "
  putStrLn"██╔══╝  ██║██║     ██║╚██╔╝██║██╔══╝  ╚════██║    ╚═╝     "
  putStrLn"██║     ██║███████╗██║ ╚═╝ ██║███████╗███████║    ██╗   \n"

-- Função para validar a senha do funcionário
validateEmployeePassword :: IO Bool
validateEmployeePassword = do
  putStrLn "Digite a senha de acesso para o modo Funcionário:"
  password <- getLine
  return (password == "admin")

-- Função para gerar relatórios de avaliação
generateReports :: IO ()
generateReports = do
  (infraAvg, foodAvg) <- calculateAverageRating
  putStrLn "Relatório de Avaliações:"
  putStrLn $ "Média das avaliações de infraestrutura: " ++ show infraAvg
  putStrLn $ "Média das avaliações de comida: " ++ show foodAvg

runEmployeeMode :: IO ()
runEmployeeMode = do
  valid <- validateEmployeePassword
  if valid
    then do
      sessionsRef <- newIORef []  -- Cria uma referência à lista de sessões
      employeeMenu sessionsRef
    else do
      putStrLn "Senha incorreta. Retornando ao menu principal."
      mainMenu -- Retorna ao menu principal

employeeMenu :: IORef [Session] -> IO ()
employeeMenu sessionsRef = do
  putStrLn "Modo Funcionário:"
  putStrLn "1) Gerenciamento de Filmes"
  putStrLn "2) Gerenciamento de Sessões"
  putStrLn "3) Administração de Usuários"
  putStrLn "4) Geração de Relatórios"
  putStrLn "5) Gerenciamento da Bomboniere"
  putStrLn "6) Voltar ao Menu Principal"
  option <- getLine
  case option of
    "1" -> manageMovies >> employeeMenu sessionsRef
    "2" -> manageSessions Employee sessionsRef >> employeeMenu sessionsRef  -- Passa 'sessionsRef' como argumento
    "3" -> manageUsers >> employeeMenu sessionsRef
    "4" -> generateReports >> employeeMenu sessionsRef -- Gera relatórios
    "5" -> manageConcessionStand >> employeeMenu sessionsRef
    "6" -> mainMenu -- Volta ao menu principal
    _   -> putStrLn "Opção inválida. Tente novamente." >> employeeMenu sessionsRef

-- Função para exibir o menu principal sem a mensagem de boas-vindas
mainMenu :: IO ()
mainMenu = do
  putStrLn "Você é: 1) Funcionário 2) Cliente 3) Sair"
  userType <- getLine
  case userType of
    "1" -> runEmployeeMode
    "2" -> runClientMode >> mainMenu -- Chama runClientMode e volta ao menu principal
    "3" -> putStrLn "Encerrando o sistema."
    _   -> putStrLn "Opção inválida. Tente novamente." >> mainMenu

-- Função principal que exibe a mensagem de boas-vindas apenas na primeira execução
main :: IO ()
main = do
  welcomeMessageShownRef <- newIORef True -- Variável de estado para controlar a exibição da mensagem de boas-vindas
  showMainMenu welcomeMessageShownRef

showMainMenu :: IORef Bool -> IO ()
showMainMenu welcomeMessageShownRef = do
  welcomeMessageShown <- readIORef welcomeMessageShownRef
  if welcomeMessageShown
    then do
      showWelcomeMessage
      writeIORef welcomeMessageShownRef False
    else return ()
  mainMenu
