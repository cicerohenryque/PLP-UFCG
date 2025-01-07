module ClientInterface where

import System.IO (withFile, IOMode(..), hGetContents)
import Data.List (isPrefixOf, find)
import SessionManagement (Session(..))
import CinemaInfo (viewCinemaInfo)
import ReviewManagement (leaveReview)
import FAQ (viewFAQ)

-- Função para visualizar filmes
viewMovies :: IO ()
viewMovies = do
  withFile "movies.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    putStrLn "Lista de filmes disponíveis:"
    putStrLn contents

-- Função para visualizar lançamentos futuros
viewUpcomingMovies :: IO ()
viewUpcomingMovies = do
  withFile "upcoming_movies.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    putStrLn "Lista de lançamentos futuros:"
    putStrLn contents

-- Função para visualizar itens da bomboniere
viewItems :: IO [String]
viewItems = do
  withFile "concessionStand.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    let items = lines contents
    putStrLn "Itens da bomboniere disponíveis:"
    if null items
      then putStrLn "Nenhum item disponível." >> return []
      else do
        mapM_ putStrLn items
        return items

-- Função para comprar um item da bomboniere
buyItem :: IO ()
buyItem = do
  items <- viewItems
  if null items
    then putStrLn "Voltando ao menu principal..."
    else do
      putStrLn "Digite o nome do item que deseja comprar:"
      itemName <- getLine
      case find (isPrefixOf itemName) items of
        Just _  -> putStrLn ("Você comprou o item: " ++ itemName)
        Nothing -> putStrLn "Item não encontrado. Voltando ao menu principal."

-- Função para visualizar sessões de cinema
viewSessions :: IO ()
viewSessions = do
  withFile "sessions.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    let sessions = read contents :: [Session]
    putStrLn "Sessões disponíveis:"
    if null sessions
      then putStrLn "Nenhuma sessão disponível."
      else mapM_ printSessionDetails sessions

-- Função auxiliar para imprimir os detalhes de uma sessão
printSessionDetails :: Session -> IO ()
printSessionDetails (Session title time room date price audio) = do
  putStrLn $ "Filme: " ++ title
  putStrLn $ "Data: " ++ date
  putStrLn $ "Horário: " ++ time
  putStrLn $ "Sala: " ++ room
  putStrLn $ "Preço do Ingresso: R$ " ++ show price
  putStrLn $ "Tipo de Áudio: " ++ audio
  putStrLn "------------------------"

-- Função para comprar um ingresso
buyTicket :: IO ()
buyTicket = do
  putStrLn "Digite o título do filme para o qual deseja comprar o ingresso:"
  movieTitle <- getLine
  putStrLn "Digite a data da sessão (DD/MM):"
  sessionDate <- getLine
  putStrLn "Digite o horário da sessão (HH:MM):"
  sessionTime <- getLine
  
  withFile "sessions.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    let sessions = read contents :: [Session]
        foundSession = find (\(Session title time _ date _ _) -> 
                              title == movieTitle && date == sessionDate && time == sessionTime) sessions

    case foundSession of
      Just (Session title time room date price audio) -> do
        putStrLn $ "Sessão encontrada!"
        putStrLn $ "Filme: " ++ title
        putStrLn $ "Data: " ++ date
        putStrLn $ "Horário: " ++ time
        putStrLn $ "Sala: " ++ room
        putStrLn $ "Preço do Ingresso: R$ " ++ show price
        putStrLn $ "Tipo de Áudio: " ++ audio
        
        putStrLn "Você é estudante e tem carteirinha? (s/n):"
        isStudent <- getLine
        let finalPrice = if isStudent == "s" then price * 0.5 else price
        putStrLn $ "Preço final do Ingresso: R$ " ++ show finalPrice

        putStrLn "Digite 's' para confirmar a compra ou qualquer outra tecla para cancelar:"
        confirmation <- getLine
        if confirmation == "s"
          then putStrLn "Ingresso comprado com sucesso!"
          else putStrLn "Compra cancelada."
      Nothing -> do
        putStrLn "Sessão não encontrada."
        putStrLn "1) Tentar novamente"
        putStrLn "2) Voltar ao menu anterior"
        option <- getLine
        case option of
          "1" -> buyTicket
          "2" -> return () -- Volta ao menu anterior
          _   -> putStrLn "Opção inválida. Voltando ao menu anterior."

-- Função para deixar feedback
giveFeedback = leaveReview

-- Menu do cliente (atualizado)
runClientMode :: IO ()
runClientMode = do
  putStrLn "Modo Cliente:"
  putStrLn "1) Visualizar Filmes"
  putStrLn "2) Visualizar Sessões Disponíveis"
  putStrLn "3) Visualizar Lançamentos Futuros"  -- Nova opção
  putStrLn "4) Comprar Ingresso"
  putStrLn "5) Comprar Item da Bomboniere"
  putStrLn "6) Visualizar Informações do Cinema"
  putStrLn "7) Deixar Feedback"
  putStrLn "8) Acessar FAQ"
  putStrLn "9) Voltar ao Menu Principal"

  option <- getLine
  case option of
    "1" -> viewMovies >> runClientMode
    "2" -> viewSessions >> runClientMode
    "3" -> viewUpcomingMovies >> runClientMode  -- Nova opção
    "4" -> buyTicket >> runClientMode
    "5" -> buyItem >> runClientMode
    "6" -> viewCinemaInfo >> runClientMode
    "7" -> giveFeedback >> runClientMode
    "8" -> viewFAQ >> runClientMode
    "9" -> return () -- Retorna ao menu principal
