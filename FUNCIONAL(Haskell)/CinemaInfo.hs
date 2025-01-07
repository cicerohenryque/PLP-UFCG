module CinemaInfo where

-- Função para obter o endereço do cinema
getAddress :: String
getAddress = "R. Irineu Joffily, 23-65 - Centro, Campina Grande. Cine Capitólio"

-- Função para obter o contato do cinema
getContact :: String
getContact = "4002-8922"

-- Função para obter o horário de funcionamento
getOperatingHours :: String
getOperatingHours = "10h00 - 00h00"

-- Função para exibir todas as informações do cinema
viewCinemaInfo :: IO ()
viewCinemaInfo = do
  putStrLn "Informações do Cinema:"
  putStrLn $ "Endereço: " ++ getAddress
  putStrLn $ "Contato: " ++ getContact
  putStrLn $ "Horário de Funcionamento: " ++ getOperatingHours