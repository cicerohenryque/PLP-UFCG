% Função para exibir o FAQ completo
view_faq :-
    write("FAQ - Perguntas Frequentes"), nl,
    write("==========================="), nl,
    write("\nEscolha uma secao para mais informacoes:"), nl,
    write("1) Programaçao"), nl,
    write("2) Ingressos"), nl,
    write("3) Voltar ao menu principal"), nl,
    read(Section),
    faq_section(Section).

% Função para gerenciar as seções do FAQ
faq_section(1) :- programming_faq.
faq_section(2) :- tickets_faq.
faq_section(3) :- !.
faq_section(_) :-
    write("Opcao invalida. Tente novamente."), nl,
    view_faq.

% Seção: Programação
programming_faq :-
    write("\nEscolha uma pergunta sobre Programacao:"), nl,
    write("1) Como faço para consultar a programacao?"), nl,
    write("2) Quando o filme possui classificacao indicativa, como proceder?"), nl,
    write("3) Por que sao exibidas mais sessoes dubladas?"), nl,
    write("4) Por quanto tempo um filme permanece em exibiçao?"), nl,
    write("5) Voltar"), nl,
    write("Caso sua duvida nao tenha sido esclarecida, envie mensagem para o numero 4002-8922"), nl,
    read(Option),
    programming_faq_option(Option).

programming_faq_option(1) :- answer_programming_faq(1), programming_faq.
programming_faq_option(2) :- answer_programming_faq(2), programming_faq.
programming_faq_option(3) :- answer_programming_faq(3), programming_faq.
programming_faq_option(4) :- answer_programming_faq(4), programming_faq.
programming_faq_option(5) :- view_faq.
programming_faq_option(_) :-
    write("Opcao invalida. Tente novamente."), nl,
    programming_faq.

% Seção: Ingressos
tickets_faq :-
    write("\nEscolha uma pergunta sobre Ingressos:"), nl,
    write("1) Como consulto o valor do ingresso?"), nl,
    write("2) Quais sao as formas de pagamento?"), nl,
    write("3) Aniversariante tem entrada gratuita?"), nl,
    write("4) Quero fechar uma sala para meus convidados. Como faco?"), nl,
    write("5) Voltar"), nl,
    write("Caso sua duvida nao tenha sido esclarecida, envie mensagem para o numero 4002-8922"), nl,
    read(Option),
    tickets_faq_option(Option).

tickets_faq_option(1) :- answer_tickets_faq(1), tickets_faq.
tickets_faq_option(2) :- answer_tickets_faq(2), tickets_faq.
tickets_faq_option(3) :- answer_tickets_faq(3), tickets_faq.
tickets_faq_option(4) :- answer_tickets_faq(4), tickets_faq.
tickets_faq_option(5) :- view_faq.
tickets_faq_option(_) :-
    write("Opcao invalida. Tente novamente."), nl,
    tickets_faq.

% Respostas para as perguntas sobre Programação
answer_programming_faq(1) :- 
    write("Para consultar a programacao, acesse nosso site ou visite a bilheteria do cinema."), nl.
answer_programming_faq(2) :- 
    write("Verifique a classificacao indicativa antes de comprar o ingresso. Respeite as restricoes de idade."), nl.
answer_programming_faq(3) :- 
    write("O motivo de exibirmos em maior demanda filmes dublados e a grande procura e preferencia da maioria do publico por este tipo de versao frente a versao legendada."), nl.
answer_programming_faq(4) :- 
    write("O que ira definir que um filme permaneça em exibicao sera a procura pelo mesmo e os acordos firmados com a Distribuidora do filme. Sendo assim, as vezes e necessario que um filme saia de exibicao para a entrada de outro lancamento."), nl.
answer_programming_faq(_) :- 
    write("Opcao invalida."), nl.

% Respostas para as perguntas sobre Ingressos
answer_tickets_faq(1) :- 
    write("O valor do ingresso pode ser consultado na bilheteria ou em nosso site."), nl.
answer_tickets_faq(2) :- 
    write("Aceitamos cartoes de credito, debito e dinheiro."), nl.
answer_tickets_faq(3) :- 
    write("Infelizmente nao temos essa politica."), nl.
answer_tickets_faq(4) :- 
    write("Para fechar uma sala, entre em contato com nossa equipe pelo número 4002-8922."), nl.
answer_tickets_faq(_) :- 
    write("Opcao invalida."), nl.
    