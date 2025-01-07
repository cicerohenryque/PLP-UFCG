% Importação dos modulos necessários
:- ensure_loaded('MovieManagement').
:- ensure_loaded('SessionManagement').
:- ensure_loaded('UserManagement').
:- ensure_loaded('ConcessionStand').
:- ensure_loaded('ReviewManagement').
:- ensure_loaded('ClientInterface').
:- ensure_loaded('FAQ').

% Exibir a mensagem de boas-vindas
show_welcome_message :-
    write(' TAPEROA FILMES! '), nl.

% Validação da senha do funcionário
validate_employee_password :-
    write('Digite a senha de acesso para o modo Funcionario: '), nl,
    read(Password),
    (Password == 'admin' -> true ; write('Senha incorreta.'), nl, fail).

% Geração de relatórios de avaliação
generate_reports :-
    calculate_average_rating(InfraAvg, FoodAvg),
    write('Relatorio de Avaliacoes:'), nl,
    write('Media das avaliacoes de infraestrutura: '), write(InfraAvg), nl,
    write('Media das avaliacoes de comida: '), write(FoodAvg), nl.

% Modo funcionário
run_employee_mode :-
    validate_employee_password,
    employee_menu.

% Menu do funcionário
employee_menu :-
    write('Modo Funcionario:'), nl,
    write('1) Gerenciamento de Filmes'), nl,
    write('2) Gerenciamento de Sessoes'), nl,
    write('3) Administracao de Usuarios'), nl,
    write('4) Geracao de Relatorios'), nl,
    write('5) Gerenciamento da Bomboniere'), nl,
    write('6) Voltar ao Menu Principal'), nl,
    read(Option),
    handle_employee_option(Option).

handle_employee_option(1) :- manage_movies, employee_menu.
handle_employee_option(2) :- manage_sessions, employee_menu.
handle_employee_option(3) :- manage_users, employee_menu.
handle_employee_option(4) :- generate_reports, employee_menu.
handle_employee_option(5) :- manage_concession_stand, employee_menu.
handle_employee_option(6) :- main_menu.
handle_employee_option(_) :-
    write('Opcao invalida. Tente novamente.'), nl,
    employee_menu.

% Menu principal
main_menu :-
    write('Voce eh: 1) Funcionario 2) Cliente 3) Sair'), nl,
    read(UserType),
    handle_main_menu_option(UserType).

handle_main_menu_option(1) :- run_employee_mode.
handle_main_menu_option(2) :- run_client_mode, main_menu.
handle_main_menu_option(3) :- write('Encerrando o sistema.'), nl.
handle_main_menu_option(_) :-
    write('Opcao invalida. Tente novamente.'), nl,
    main_menu.

% Função principal (inicial)
main :-
    show_welcome_message,
    main_menu.

% Alias para a função principal
iniciar :- main.
