:- use_module(library(readutil)).
:- dynamic item/2, session/6.

% Importando os modulos
:- consult('SessionManagement').
:- consult('CinemaInfo').
:- consult('ReviewManagement').
:- consult('faq').

% Função para visualizar filmes
view_movies_1 :-
    open('movies.txt', read, Stream),
    read_string(Stream, _, Contents),
    writeln("Lista de filmes disponíveis:"),
    writeln(Contents),
    close(Stream).

% Função para visualizar lançamentos futuros
view_upcoming_movies :-
    open('upcoming_movies.txt', read, Stream),
    read_string(Stream, _, Contents),
    writeln("Lista de lançamentos futuros:"),
    writeln(Contents),
    close(Stream).

% Função para visualizar itens da bomboniere
view_items :-
    findall((Nome, Preco), item(Nome, Preco), Itens),
    (Itens == [] ->
        writeln("Nenhum item disponível.");
        writeln("Itens da bomboniere disponíveis:"),
        print_items(Itens)).

print_items_([]).
print_items([(Nome, Preco)|Rest]) :-
    format("Item: ~w | Preço: R$ ~w~n", [Nome, Preco]),
    print_items(Rest).

% Função para comprar um item da bomboniere
buy_item :-
    view_items,
    writeln("Digite o nome do item que deseja comprar:"),
    read(ItemName),
    (item(ItemName, _) ->
        format("Você comprou o item: ~w~n", [ItemName]);
        writeln("Item não encontrado. Voltando ao menu principal.")).

% Função para visualizar sessões de cinema
view_sessions_1 :-
    findall((Titulo, Horario, Sala, Data, Preco, Audio), session(Titulo, Horario, Sala, Data, Preco, Audio), Sessoes),
    (Sessoes == [] ->
        writeln("Nenhuma sessão disponível.");
        writeln("Sessões disponíveis:"),
        print_sessions(Sessoes)).

% Função auxiliar para imprimir os detalhes de uma sessão
print_sessions([]).
print_sessions([(Titulo, Horario, Sala, Data, Preco, Audio) | Rest]) :-
    format("Filme: ~w~n", [Titulo]),
    format("Data: ~w~n", [Data]),
    format("Horário: ~w~n", [Horario]),
    format("Sala: ~w~n", [Sala]),
    format("Preço do Ingresso: R$ ~w~n", [Preco]),
    format("Tipo de Áudio: ~w~n", [Audio]),
    writeln("------------------------"),
    print_sessions(Rest).

% Função para comprar um ingresso
buy_ticket :-
    writeln("Digite o título do filme para o qual deseja comprar o ingresso:"),
    read(MovieTitle),
    writeln("Digite a data da sessão (DD/MM):"),
    read(SessionDate),
    writeln("Digite o horário da sessão (HH:MM):"),
    read(SessionTime),
    (session(MovieTitle, SessionTime, Sala, SessionDate, Preco, Audio) ->
        format("Sessão encontrada!~n"),
        format("Filme: ~w~n", [MovieTitle]),
        format("Data: ~w~n", [SessionDate]),
        format("Horário: ~w~n", [SessionTime]),
        format("Sala: ~w~n", [Sala]),
        format("Preço do Ingresso: R$ ~w~n", [Preco]),
        format("Tipo de Áudio: ~w~n", [Audio]),
        writeln("Você é estudante e tem carteirinha? (s/n):"),
        read(IsStudent),
        (IsStudent == "s" ->
            FinalPrice is Preco * 0.5;
            FinalPrice is Preco),
        format("Preço final do Ingresso: R$ ~w~n", [FinalPrice]),
        writeln("Digite 's' para confirmar a compra ou qualquer outra tecla para cancelar:"),
        read(Confirmation),
        (Confirmation == "s" ->
            writeln("Ingresso comprado com sucesso!");
            writeln("Compra cancelada."));
        writeln("Sessão não encontrada.")).

% Função para deixar feedback
give_feedback :-
    writeln("Digite seu feedback:"),
    read_line_to_string(user_input, Feedback),
    open('reviews.txt', append, Stream),
    writeln(Stream, Feedback),
    close(Stream),
    writeln("Obrigado pelo seu feedback!").

% Função auxiliar para ler o conteúdo de um arquivo
read_file(File, Contents) :-
    open(File, read, Stream),
    read_string(Stream, _, Contents),
    close(Stream).

% Função auxiliar para ler linhas de um arquivo
read_file_lines(File, Lines) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    close(Stream).

% Menu do cliente
run_client_mode :-
    writeln("Modo Cliente:"),
    writeln("1) Visualizar Filmes"),
    writeln("2) Visualizar Sessões Disponíveis"),
    writeln("3) Visualizar Lançamentos Futuros"),
    writeln("4) Comprar Ingresso"),
    writeln("5) Comprar Item da Bomboniere"),
    writeln("6) Visualizar Informações do Cinema"),
    writeln("7) Deixar Feedback"),
    writeln("8) Acessar FAQ"),
    writeln("9) Voltar ao Menu Principal"),
    read(Option),
    handle_option(Option).

handle_option(1) :- view_movies_1, run_client_mode.
handle_option(2) :- view_sessions_1, run_client_mode.
handle_option(3) :- view_upcoming_movies, run_client_mode.
handle_option(4) :- buy_ticket, run_client_mode.
handle_option(5) :- buy_item, run_client_mode.
handle_option(6) :- view_cinema_info, run_client_mode.
handle_option(7) :- give_feedback, run_client_mode.
handle_option(8) :- view_faq, run_client_mode.
handle_option(9) :- writeln("Voltando ao menu principal.").
handle_option(_) :- writeln("Opção inválida. Tente novamente."), run_client_mode.
