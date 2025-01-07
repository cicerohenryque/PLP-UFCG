:- dynamic movie/6.  % Para permitir a adição e remoção dinâmica de filmes

% Adiciona um filme ao arquivo e à base de dados dinâmica
add_movie(ID, Title, Director, Year, Classification, Duration) :-
    assertz(movie(ID, Title, Director, Year, Classification, Duration)),  % Adiciona ao banco de dados dinâmico
    open('movies.txt', append, Stream),
    write(Stream, ID), write(Stream, ';'),
    write(Stream, Title), write(Stream, ';'),
    write(Stream, Director), write(Stream, ';'),
    write(Stream, Year), write(Stream, ';'),
    write(Stream, Classification), write(Stream, ';'),
    write(Stream, Duration), write(Stream, '\n'),
    close(Stream),
    write('Filme adicionado com sucesso!'), nl.

% Lista todos os filmes do arquivo
list_movies :-
    open('movies.txt', read, Stream),
    repeat,
    read_line_to_string(Stream, Line),
    ( Line \= end_of_file ->
        split_string(Line, ";", "", [IDStr, TitleStr, DirectorStr, YearStr, ClassificationStr, DurationStr]),
        format("ID: ~s, Título: ~s, Diretor: ~s, Ano: ~s, Classificação: ~s, Duração: ~s~n", 
               [IDStr, TitleStr, DirectorStr, YearStr, ClassificationStr, DurationStr]),
        fail
    ;   close(Stream)
    ).

% Remove um filme do arquivo
remove_movie(RemoveID) :-
    read_file_to_string('movies.txt', Content, []),
    split_string(Content, "\n", "", Lines),
    exclude(contains_id(RemoveID), Lines, UpdatedLines),  % Remove a linha com o ID correspondente
    open('movies.txt', write, Stream),
    forall(member(Line, UpdatedLines), (write(Stream, Line), write(Stream, '\n'))),
    close(Stream),
    write('Filme removido com sucesso!'), nl.

% Predicado auxiliar para verificar se a linha contém o ID do filme a ser removido
contains_id(ID, Line) :-
    sub_string(Line, 0, _, _, ID).

% Edita um filme no arquivo
edit_movie(EditID, NewTitle, NewDirector, NewYear, NewClassification, NewDuration) :-
    read_file_to_string('movies.txt', Content, []),
    split_string(Content, "\n", "", Lines),
    findall(
        NewLine,
        (member(Line, Lines),
         (sub_string(Line, 0, _, _, EditID) ->
             format(string(NewLine), "~s;~s;~s;~s;~s;~s", 
                    [EditID, NewTitle, NewDirector, NewYear, NewClassification, NewDuration])  % Atualiza a linha
         ;   NewLine = Line)),  % Mantém a linha original
        UpdatedLines
    ),
    open('movies.txt', write, Stream),
    forall(member(Line, UpdatedLines), (write(Stream, Line), write(Stream, '\n'))),
    close(Stream),
    write('Filme editado com sucesso!'), nl.

% Menu de gerenciamento de filmes
manage_movies :-
    write('Gerenciamento de Filmes:'), nl,
    write('1) Adicionar Filme'), nl,
    write('2) Listar Filmes'), nl,
    write('3) Editar Filme'), nl,
    write('4) Remover Filme'), nl,
    write('5) Voltar ao Menu Funcionário'), nl,
    read(Option),
    handle_movie_option(Option).

handle_movie_option(1) :-
    write('Digite o ID do filme: '), read(ID),
    write('Digite o título do filme: '), read(Title),
    write('Digite o diretor do filme: '), read(Director),
    write('Digite o ano do filme: '), read(Year),
    write('Digite a classificação do filme: '), read(Classification),
    write('Digite a duração do filme (em minutos): '), read(Duration),
    add_movie(ID, Title, Director, Year, Classification, Duration),
    manage_movies.
handle_movie_option(2) :-
    list_movies,
    manage_movies.
handle_movie_option(3) :-
    write('Digite o ID do filme a ser editado: '), read(EditID),
    write('Digite o novo título do filme: '), read(NewTitle),
    write('Digite o novo diretor do filme: '), read(NewDirector),
    write('Digite o novo ano do filme: '), read(NewYear),
    write('Digite a nova classificação do filme: '), read(NewClassification),
    write('Digite a nova duração do filme (em minutos): '), read(NewDuration),
    edit_movie(EditID, NewTitle, NewDirector, NewYear, NewClassification, NewDuration),
    manage_movies.
handle_movie_option(4) :-
    write('Digite o ID do filme a ser removido: '), read(RemoveID),
    remove_movie(RemoveID),
    manage_movies.
handle_movie_option(5) :-
    write('Voltando ao Menu Funcionário...'), nl.