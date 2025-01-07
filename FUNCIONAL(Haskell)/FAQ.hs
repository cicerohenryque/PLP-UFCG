module FAQ where

-- Função para exibir o FAQ completo
viewFAQ :: IO ()
viewFAQ = do
    putStrLn "FAQ - Perguntas Frequentes"
    putStrLn "==========================="
    putStrLn "\nEscolha uma seção para mais informações:"
    putStrLn "1) Programação"
    putStrLn "2) Ingressos"
    putStrLn "3) Voltar ao menu principal"
    section <- getLine
    case section of
        "1" -> programmingFAQ
        "2" -> ticketsFAQ
        "3" -> return ()
        _   -> putStrLn "Opção inválida. Tente novamente." >> viewFAQ

-- Seção: Programação
programmingFAQ :: IO ()
programmingFAQ = do
    putStrLn "\nEscolha uma pergunta sobre Programação:"
    putStrLn "1) Como faço para consultar a programação?"
    putStrLn "2) Quando o filme possui classificação indicativa, como proceder?"
    putStrLn "3) Por que são exibidas mais sessões dubladas?"
    putStrLn "4) Por quanto tempo um filme permanece em exibição?"
    putStrLn "5) Voltar"
    putStrLn "Caso sua dúvida não tenha sido esclarecida, envie mensagem para o número 4002-8922"
    option <- getLine
    case option of
        "1" -> answerProgrammingFAQ 1 >> programmingFAQ
        "2" -> answerProgrammingFAQ 2 >> programmingFAQ
        "3" -> answerProgrammingFAQ 3 >> programmingFAQ
        "4" -> answerProgrammingFAQ 4 >> programmingFAQ
        "5" -> viewFAQ
        _   -> putStrLn "Opção inválida. Tente novamente." >> programmingFAQ

-- Seção: Ingressos
ticketsFAQ :: IO ()
ticketsFAQ = do
    putStrLn "\nEscolha uma pergunta sobre Ingressos:"
    putStrLn "1) Como consulto o valor do ingresso?"
    putStrLn "2) Quais são as formas de pagamento?"
    putStrLn "3) Aniversariante tem entrada gratuita?"
    putStrLn "4) Quero fechar uma sala para meus convidados. Como faço?"
    putStrLn "5) Voltar"
    putStrLn "Caso sua dúvida não tenha sido esclarecida, envie mensagem para o número 4002-8922"
    option <- getLine
    case option of
        "1" -> answerTicketsFAQ 1 >> ticketsFAQ
        "2" -> answerTicketsFAQ 2 >> ticketsFAQ
        "3" -> answerTicketsFAQ 3 >> ticketsFAQ
        "4" -> answerTicketsFAQ 4 >> ticketsFAQ
        "5" -> viewFAQ
        _   -> putStrLn "Opção inválida. Tente novamente." >> ticketsFAQ

-- Respostas para as perguntas sobre Programação
answerProgrammingFAQ :: Int -> IO ()
answerProgrammingFAQ 1 = putStrLn "Para consultar a programação, acesse nosso site ou visite a bilheteria do cinema."
answerProgrammingFAQ 2 = putStrLn "Verifique a classificação indicativa antes de comprar o ingresso. Respeite as restrições de idade."
answerProgrammingFAQ 3 = putStrLn "O motivo de exibirmos em maior demanda filmes dublados é a grande procura e preferência da maioria do público por este tipo de versão frente à versão legendada."
answerProgrammingFAQ 4 = putStrLn "O que irá definir que um filme permaneça em exibição será a procura pelo mesmo e os acordos firmados com a Distribuidora do filme. Sendo assim, às vezes é necessário que um filme saia de exibição para a entrada de outro lançamento."
answerProgrammingFAQ _ = putStrLn "Opção inválida."

-- Respostas para as perguntas sobre Ingressos
answerTicketsFAQ :: Int -> IO ()
answerTicketsFAQ 1 = putStrLn "O valor do ingresso pode ser consultado na bilheteria ou em nosso site."
answerTicketsFAQ 2 = putStrLn "Aceitamos cartões de crédito, débito e dinheiro."
answerTicketsFAQ 3 = putStrLn "Infelizmente não temos essa política."
answerTicketsFAQ 4 = putStrLn "Para fechar uma sala, entre em contato com nossa equipe pelo número 4002-8922."
answerTicketsFAQ _ = putStrLn "Opção inválida."