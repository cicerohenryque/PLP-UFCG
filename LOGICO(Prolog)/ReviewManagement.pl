:- use_module(library(readutil)).
:- use_module(library(files)).
:- use_module(library(lists)).

% Função para deixar uma avaliação
leave_review :-
    write("Avalie a infraestrutura da sala (nota de 0 a 10):"), nl,
    read(InfrastructureRating),
    write("Avalie a comida (nota de 0 a 10):"), nl,
    read(FoodRating),
    format(string(ReviewEntry), "Infraestrutura: ~w | Comida: ~w~n", [InfrastructureRating, FoodRating]),
    open('reviews.txt', append, Stream),
    write(Stream, ReviewEntry),
    close(Stream),
    write("Avaliação registrada com sucesso."), nl.

% Função para calcular a media das avaliações
calculate_average_rating(InfraAvg, FoodAvg) :-
    read_file_to_string('reviews.txt', Content, []),
    split_string(Content, "\n", "", Reviews),
    findall(Rating, (member(Line, Reviews), sub_string(Line, _, _, _, "Infraestrutura:"), parse_rating(Line, Rating)), InfraRatings),
    findall(Rating, (member(Line, Reviews), sub_string(Line, _, _, _, "Comida:"), parse_rating(Line, Rating)), FoodRatings),
    ( InfraRatings \= [] -> sum_list(InfraRatings, InfraSum), length(InfraRatings, InfraCount), InfraAvg is InfraSum / InfraCount ; InfraAvg is 0 ),
    ( FoodRatings \= [] -> sum_list(FoodRatings, FoodSum), length(FoodRatings, FoodCount), FoodAvg is FoodSum / FoodCount ; FoodAvg is 0 ).

% Função auxiliar para parsear a avaliação
parse_rating(Line, Rating) :-
    split_string(Line, " ", "", [_ | [RatingStr | _]]),
    atom_number(RatingStr, Rating).