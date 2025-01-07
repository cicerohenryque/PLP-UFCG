module ReviewManagement where

import System.IO (appendFile, readFile)
import Data.List (isInfixOf, words)
import Text.Read (readMaybe) -- Para parsing seguro

-- Função para deixar uma avaliação
leaveReview :: IO ()
leaveReview = do
    putStrLn "Avalie a infraestrutura da sala (nota de 0 a 10):"
    infraRating <- getLine
    putStrLn "Avalie a comida (nota de 0 a 10):"
    foodRating <- getLine

    let reviewEntry = "Infraestrutura: " ++ infraRating ++ " | Comida: " ++ foodRating ++ "\n"
    appendFile "reviews.txt" reviewEntry
    putStrLn "Avaliação registrada com sucesso."

-- Função para calcular a média das avaliações
calculateAverageRating :: IO (Float, Float)
calculateAverageRating = do
    contents <- readFile "reviews.txt"
    let reviews = lines contents
        parseRating line = case words line of
            (_:ratingStr:_) -> readMaybe ratingStr :: Maybe Float
            _ -> Nothing
        infraRatings = [r | line <- reviews, "Infraestrutura:" `isInfixOf` line, Just r <- [parseRating line]]
        foodRatings = [r | line <- reviews, "Comida:" `isInfixOf` line, Just r <- [parseRating line]]
        infraAvg = if null infraRatings then 0 else sum infraRatings / fromIntegral (length infraRatings)
        foodAvg = if null foodRatings then 0 else sum foodRatings / fromIntegral (length foodRatings)
    return (infraAvg, foodAvg)