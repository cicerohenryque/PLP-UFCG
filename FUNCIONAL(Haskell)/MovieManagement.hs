module MovieManagement where

import System.IO (appendFile, readFile, writeFile)
import Data.List (isPrefixOf, isInfixOf, intercalate, delete)

-- Função para adicionar um filme
addMovie :: IO ()
addMovie = do
  putStrLn "Digite o título do filme:"
  title <- getLine
  putStrLn "Digite o diretor do filme:"
  director <- getLine
  putStrLn "Digite o ano de lançamento:"
  year <- getLine
  putStrLn "Digite a classificação indicativa:"
  rating <- getLine
  putStrLn "Digite a duração do filme (em minutos):"
  duration <- getLine
  let movieEntry = "Filme em cartaz: " ++ title ++ " | " ++ director ++ " | " ++ year ++ " | " ++ rating ++ " | " ++ duration ++ " min\n"
  appendFile "movies.txt" movieEntry
  putStrLn "Filme adicionado com sucesso."

-- Função para adicionar um filme que ainda vai entrar em cartaz
addUpcomingMovie :: IO ()
addUpcomingMovie = do
  putStrLn "Digite o título do filme (lançamento futuro):"
  title <- getLine
  putStrLn "Digite o diretor do filme:"
  director <- getLine
  putStrLn "Digite o ano previsto de lançamento:"
  releaseYear <- getLine
  putStrLn "Digite a classificação indicativa:"
  rating <- getLine
  putStrLn "Digite a duração estimada do filme (em minutos):"
  duration <- getLine
  putStrLn "Digite a data prevista de estreia (formato DD/MM):"
  releaseDate <- getLine
  let fullReleaseDate = releaseDate ++ "/" ++ releaseYear
  let upcomingMovieEntry = "Chega em breve: " ++ title ++ " | " ++ director ++ " | " ++ fullReleaseDate ++ " | " ++ rating ++ " | " ++ duration ++ " min\n"
  appendFile "upcoming_movies.txt" upcomingMovieEntry
  putStrLn "Filme futuro adicionado com sucesso."

-- Função para listar todos os filmes em cartaz
listMovies :: IO [String]
listMovies = do
  contents <- readFile "movies.txt"
  let movies = filter (isPrefixOf "Filme em cartaz: ") (lines contents)
  putStrLn "Lista de filmes disponíveis:"
  mapM_ putStrLn movies
  return movies

-- Função para listar lançamentos futuros
listUpcomingMovies :: IO [String]
listUpcomingMovies = do
  contents <- readFile "upcoming_movies.txt"
  let upcomingMovies = filter (isPrefixOf "Chega em breve: ") (lines contents)
  putStrLn "Lista de filmes que entrarão em cartaz:"
  mapM_ putStrLn upcomingMovies
  return upcomingMovies

-- Função para editar um filme em cartaz
editMovie :: IO ()
editMovie = do
  movies <- listMovies
  putStrLn "Digite o título do filme a ser editado:"
  titleToEdit <- getLine
  let movieToEdit = filter (isInfixOf titleToEdit) movies
  if null movieToEdit
    then putStrLn "Filme não encontrado."
    else do
      putStrLn "Digite o novo título do filme:"
      newTitle <- getLine
      putStrLn "Digite o novo diretor do filme:"
      newDirector <- getLine
      putStrLn "Digite o novo ano de lançamento:"
      newYear <- getLine
      putStrLn "Digite a nova classificação indicativa:"
      newRating <- getLine
      putStrLn "Digite a nova duração do filme (em minutos):"
      newDuration <- getLine
      let newMovieEntry = "Filme em cartaz: " ++ newTitle ++ " | " ++ newDirector ++ " | " ++ newYear ++ " | " ++ newRating ++ " | " ++ newDuration ++ " min"
      let updatedMovies = map (\movie -> if isInfixOf titleToEdit movie then newMovieEntry else movie) movies
      writeFile "movies.txt" (unlines(updatedMovies))
      putStrLn "Filme editado com sucesso."


-- Função para remover um filme
removeMovie :: IO ()
removeMovie = do
  movies <- listMovies
  putStrLn "Digite o título do filme a ser removido:"
  titleToRemove <- getLine
  let updatedMovies = filter (not . isInfixOf titleToRemove) movies
  writeFile "movies.txt" (unlines(updatedMovies))
  putStrLn "Filme removido com sucesso."


-- Função para remover filmes que AINDA entrarão em cartaz
removeUpcomingMovie :: IO ()
removeUpcomingMovie = do
  upcomingMovies <- listUpcomingMovies
  putStrLn "Digite o título do lançamento futuro a ser removido:"
  titleToRemove <- getLine
  let updatedMovies = filter (not . isInfixOf titleToRemove) upcomingMovies
  writeFile "upcoming_movies.txt" (unlines updatedMovies)
  putStrLn "Lançamento futuro removido com sucesso."


-- Função para editar filmes em pré lançamento
editUpcomingMovie :: IO ()
editUpcomingMovie = do
  upcomingMovies <- listUpcomingMovies
  putStrLn "Digite o título do lançamento futuro a ser editado:"
  titleToEdit <- getLine
  let movieToEdit = filter (isInfixOf titleToEdit) upcomingMovies
  if null movieToEdit
    then putStrLn "Lançamento futuro não encontrado."
    else do
      putStrLn "Digite o novo título do filme (lançamento futuro):"
      newTitle <- getLine
      putStrLn "Digite o novo diretor do filme:"
      newDirector <- getLine
      putStrLn "Digite o novo ano previsto de lançamento:"
      newReleaseYear <- getLine
      putStrLn "Digite a nova classificação indicativa:"
      newRating <- getLine
      putStrLn "Digite a nova duração estimada do filme (em minutos):"
      newDuration <- getLine
      putStrLn "Digite a nova data prevista de estreia (formato DD/MM):"
      newReleaseDate <- getLine
      let fullReleaseDate = newReleaseDate ++ "/" ++ newReleaseYear
      let newMovieEntry = "Chega em breve: " ++ newTitle ++ " | " ++ newDirector ++ " | " ++ fullReleaseDate ++ " | " ++ newRating ++ " | " ++ newDuration ++ " min"
      let updatedMovies = map (\movie -> if isInfixOf titleToEdit movie then newMovieEntry else movie) upcomingMovies
      writeFile "upcoming_movies.txt" (unlines updatedMovies)
      putStrLn "Lançamento futuro editado com sucesso."


manageMovies :: IO ()
manageMovies = do
  putStrLn "Gerenciamento de Filmes:"
  putStrLn "1) Adicionar Filme (em cartaz)"
  putStrLn "2) Adicionar lançamento futuro"
  putStrLn "3) Editar Filme em cartaz"
  putStrLn "4) Editar Lançamento Futuro"
  putStrLn "5) Listar Filmes em cartaz"
  putStrLn "6) Listar Lançamentos Futuros"
  putStrLn "7) Remover Filme em cartaz"
  putStrLn "8) Remover Lançamento Futuro"
  putStrLn "9) Voltar"
  option <- getLine
  case option of
    "1" -> addMovie >> manageMovies
    "2" -> addUpcomingMovie >> manageMovies
    "3" -> editMovie >> manageMovies
    "4" -> editUpcomingMovie >> manageMovies
    "5" -> listMovies >> manageMovies
    "6" -> listUpcomingMovies >> manageMovies
    "7" -> removeMovie >> manageMovies
    "8" -> removeUpcomingMovie >> manageMovies
    "9" -> putStrLn "Voltando ao menu anterior."
    _   -> putStrLn "Opção inválida. Tente novamente." >> manageMovies
