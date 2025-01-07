% Função para obter o endereço do cinema
get_address("R. Irineu Joffily, 23-65 - Centro, Campina Grande. Cine Capitolio").

% Função para obter o contato do cinema
get_contact("4002-8922").

% Função para obter o horário de funcionamento
get_operating_hours("10h00 - 00h00").

% Função para exibir todas as informações do cinema
view_cinema_info :-
    get_address(Address),
    get_contact(Contact),
    get_operating_hours(OperatingHours),
    write("Informacoes do Cinema:"), nl,
    write("Endereco: "), write(Address), nl,
    write("Contato: "), write(Contact), nl,
    write("Horario de Funcionamento: "), write(OperatingHours), nl.

