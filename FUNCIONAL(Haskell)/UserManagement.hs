module UserManagement where

import System.IO (appendFile, readFile, writeFile)
import Data.List (isInfixOf)

-- Função para adicionar um usuário
addUser :: IO ()
addUser = do
  putStrLn "Digite o e-mail do usuário:"
  email <- getLine
  putStrLn "Digite a pontuação do usuário:"
  points <- getLine
  let userEntry = "Usuario: " ++ email ++ " | Pontos: " ++ points ++ "\n"
  appendFile "usersCinema.txt" userEntry
  putStrLn "Usuário adicionado com sucesso."

-- Lista os usuários para facilitar a visualização na hora de remover
listUsers :: IO [String]
listUsers = do
  contents <- readFile "usersCinema.txt"
  let users = filter (isInfixOf "Usuário: ") (lines contents)
  putStrLn "Lista de usuários:"
  mapM_ putStrLn users
  return users

-- Função para editar um usuário existente
editUser :: IO ()
editUser = do
  users <- listUsers
  putStrLn "Digite o e-mail do usuário a ser editado:"
  emailToEdit <- getLine
  let userToEdit = filter (isInfixOf emailToEdit) users
  if null userToEdit
    then putStrLn "Usuário não encontrado."
    else do
      putStrLn "Digite o novo e-mail do usuário:"
      newEmail <- getLine
      putStrLn "Digite a nova pontuação do usuário:"
      newPoints <- getLine
      let newUserEntry = "Usuario: " ++ newEmail ++ " | Pontos: " ++ newPoints
      let updatedUsers = map (\user -> if isInfixOf emailToEdit user then newUserEntry else user) users
      writeFile "usersCinema.txt" (unlines updatedUsers)
      putStrLn "Usuário editado com sucesso."

-- Função para remover um usuário
removeUser :: IO ()
removeUser = do
  users <- listUsers
  putStrLn "Digite o e-mail do usuário a ser removido:"
  emailToRemove <- getLine
  let userToRemove = filter (isInfixOf emailToRemove) users
  if null userToRemove
    then putStrLn "Usuário não encontrado."
    else do
      let updatedUsers = filter (not . isInfixOf emailToRemove) users
      writeFile "usersCinema.txt" (unlines updatedUsers)
      putStrLn "Usuário removido com sucesso."


-- Função para gerenciar usuários
manageUsers :: IO ()
manageUsers = do
  putStrLn "Gerenciamento de Usuários:"
  putStrLn "1) Adicionar Usuário"
  putStrLn "2) Editar Usuário"
  putStrLn "3) Remover Usuário"
  putStrLn "4) Voltar"
  option <- getLine
  case option of
    "1" -> addUser >> manageUsers
    "2" -> editUser >> manageUsers
    "3" -> removeUser >> manageUsers
    "4" -> putStrLn "Voltando ao menu anterior."
    _   -> putStrLn "Opção inválida. Tente novamente." >> manageUsers
