:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(strings)).
:- dynamic user/2.
:- discontiguous print_users/1.

% Função para adicionar um usuário
add_user :-
    write('Digite o nome do usuario: '),
    read(Email),
    write('Digite a pontuacao do usuario: '),
    read(Points),
    open('usersCinema.txt', append, Stream),
    format(Stream, 'Usuario: ~w | Pontos: ~w\n', [Email, Points]),
    close(Stream),
    write('Usuario adicionado com sucesso.\n').

% Função para listar os usuários
list_users(Users) :-
    open('usersCinema.txt', read, Stream),
    read_lines_1(Stream, Users),
    close(Stream),
    write('Lista de usuarios:\n'),
    print_users(Users).

% Auxiliar para imprimir os usuários
print_users([]).
print_users([(Email, Points)|T]) :-
    format("Usuario: ~w | Pontos: ~w", [Email, Points]), nl,
    print_users(T).

read_lines_1(Stream, []) :-
    at_end_of_stream(Stream).

read_lines_1(Stream, [X|L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, X),
    read_lines_1(Stream, L).

% Função para imprimir usuários
print_users([]).
print_users([User|Rest]) :-
    write(User), nl,
    print_users(Rest).

% Função para editar um usuário existente
edit_user :-
    list_users(Users),
    write('Digite o nome do usuario a ser editado: '),
    read(EmailToEdit),
    (   member(User, Users), sub_string(User, _, _, _, EmailToEdit)
    ->  write('Digite o novo nome do usuario: '),
        read(NewEmail),
        write('Digite a nova pontuacao do usuario: '),
        read(NewPoints),
        format(string(NewUser), 'Usuario: ~w | Pontos: ~w', [NewEmail, NewPoints]),
        select(User, Users, NewUser, UpdatedUsers),
        write_users(UpdatedUsers),
        write('Usuario editado com sucesso.\n')
    ;   write('Usuario nao encontrado.\n')).

% Função para remover um usuário
remove_user :-
    list_users(Users),
    write('Digite o nome do usuário a ser removido: '),
    read(EmailToRemove),
    (   member(User, Users), sub_string(User, _, _, _, EmailToRemove)
    ->  delete(Users, User, UpdatedUsers),
        write_users(UpdatedUsers),
        write('Usuario removido com sucesso.\n')
    ;   write('Usuario nao encontrado.\n')).

write_users(Users) :-
    open('usersCinema.txt', write, Stream),
    write_users_to_file(Stream, Users),
    close(Stream).

write_users_to_file(_, []).
write_users_to_file(Stream, [User|Rest]) :-
    writeln(Stream, User),
    write_users_to_file(Stream, Rest).

% Função para gerenciar usuários
manage_users :-
    write('Gerenciamento de Usuarios:\n'),
    write('1) Adicionar Usuario\n'),
    write('2) Editar Usuario\n'),
    write('3) Remover Usuario\n'),
    write('4) Voltar\n'),
    read(Option),
    (   Option == 1 -> add_user, manage_users
    ;   Option == 2 -> edit_user, manage_users
    ;   Option == 3 -> remove_user, manage_users
    ;   Option == 4 -> write('Voltando ao menu anterior.\n')
    ;   write('Opcao invalida. Tente novamente.\n'), manage_users).

/*% Função para carregar usuários do arquivo
load_users :-
    open('usersCinema.txt', read, Stream),
    repeat,
    read_line_to_string(Stream, Line),
    (   Line == end_of_file ->
        close(Stream), !
    ;   split_string(Line, ":| ", "", Parts),
        nth1(2, Parts, Email),
        nth1(4, Parts, PointsString),
        number_string(Points, PointsString),
        assertz(user(Email, Points)),
        fail
    ).*/