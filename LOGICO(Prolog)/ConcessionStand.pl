:- discontiguous read_file/2.
:- discontiguous read_file_1/2.

% Arquivo onde os itens da bomboniere são armazenados
file_name('concessionStand.txt').

% Código de acesso para o funcionário (você pode mudar para algo mais seguro)
employee_password('admin').

% Função para adicionar um item à bomboniere
add_item :-
    write('Digite o nome do item: \n'),
    read(ItemName),
    write('Digite o preço do item (em reais): \n'),
    read(Price),
    atomics_to_string([ItemName, ' | R$ ', Price, '\n'], ItemEntry),
    open('concessionStand.txt', append, Stream),
    write(Stream, ItemEntry),
    close(Stream),
    write('Item adicionado com sucesso. \n').


% Função para listar todos os itens da bomboniere
list_items :-
    file_name(FileName),
    open(FileName, read, Stream),
    read_lines(Stream, Items),
    close(Stream),
    ( Items = [] ->
        write("Nenhum item disponivel na bomboniere."), nl
    ; write("Itens da bomboniere:"), nl,
      maplist(display_item, Items)
    ).

% Evita a redefinição do write_item/1 em outros arquivos
display_item(Item) :-
    write(Item), nl.

% Função auxiliar para imprimir os itens
print_itens([]).
print_itens([Item|Items]) :-
    write(Item), nl,
    print_itens(Items).


% Função auxiliar para ler o conteúdo de um arquivo
read_file_1(Stream, Contents) :-
    read_string(Stream, _, Contents).

% Função para editar um item na bomboniere
edit_item :-
    list_items,
    write('Digite o nome do item a ser editado: \n'),
    read(OldName),
    write('Digite o novo nome do item (ou deixe em branco para manter o mesmo): \n'),
    read(NewName),
    write('Digite o novo preço do item (ou deixe em branco para manter o mesmo): \n'),
    read(NewPrice),
    read_file_1('concessionStand.txt', Items),
    update_items(OldName, NewName, NewPrice, Items, UpdatedItems),
    open('concessionStand.txt', write, Stream),
    write_file(Stream, UpdatedItems),
    close(Stream),
    write('Item atualizado com sucesso.').

% Função auxiliar para atualizar os itens
update_items(_, _, _, [], []).
update_items(OldName, NewName, NewPrice, [Item|Items], UpdatedItems) :-
    (   sub_string(Item, 0, _, _, OldName)
    ->  update_item(Item, NewName, NewPrice, UpdatedItem),
        UpdatedItems = [UpdatedItem|UpdatedItems1],
        update_items(OldName, NewName, NewPrice, Items, UpdatedItems1)
    ;   UpdatedItems = [Item|UpdatedItems1],
        update_items(OldName, NewName, NewPrice, Items, UpdatedItems1)
    ).

/*% Função auxiliar para atualizar um item
update_item(Item, NewName, NewPrice, UpdatedItem) :-
    sub_string(Item, _, _, _, '|'),
    sub_string(Item, _, _, _, 'R$'),
    (   NewName \= ''
    ->  UpdatedItem = NewName
    ;   UpdatedItem = Item
    ),
    (   NewPrice \= ''
    ->  UpdatedItem = UpdatedItem ++ ' | R$ ' ++ NewPrice
    ;   UpdatedItem = UpdatedItem ++ ' | R$ ' ++ Item
    ).*/


% Função para remover um item da bomboniere (somente funcionário)
remove_item :-
    write("Digite o codigo de acesso do funcionario:"), nl,
    read(EnteredPassword),
    employee_password(Password),
    ( EnteredPassword = Password ->
        open('concessionStand.txt', read, Stream),
        read_lines(Stream, Items),
        close(Stream),
        write('Itens da bomboniere:'), nl,
        print_itens(Items),
        write('Digite o nome do item a ser removido: \n'),
        read(ItemToRemove),
        remove_item_from_list(ItemToRemove, Items, UpdatedItems),
        ( UpdatedItems = Items ->
            write('Item não encontrado na lista.')
        ; open('concessionStand.txt', write, NewStream),
            write_file(NewStream, UpdatedItems),
            close(NewStream),
            write('Item removido com sucesso.')
        )
    ; write('Código de acesso incorreto. Acesso negado.')
    ).

% Função auxiliar para remover um item da lista
remove_item_from_list(_, [], []).
remove_item_from_list(ItemToRemove, [Item|Items], UpdatedItems) :-
    (   sub_string(Item, 0, _, _, ItemToRemove)
    ->  UpdatedItems = UpdatedItems1,
        remove_item_from_list(ItemToRemove, Items, UpdatedItems1)
    ;   UpdatedItems = [Item|UpdatedItems1],
        remove_item_from_list(ItemToRemove, Items, UpdatedItems1)
    ).

% Função para comprar itens da bomboniere
buy_items :-
    list_items,
    write('Digite o nome do item que deseja comprar (ou \'voltar\' para retornar ao menu principal): \n'),
    read(ItemToBuy),
    (   ItemToBuy = 'voltar'
    ->  true
    ;   write('Você comprou: '), write(ItemToBuy), nl
    ).

% Menu de gerenciamento da bomboniere
manage_concession_stand :-
    write('1) Adicionar Item'), nl,
    write('2) Listar Itens'), nl,
    write('3) Editar Item'), nl,
    write('4) Remover Item'), nl,
    write('5) Voltar ao Menu Principal'), nl,
    write('Digite a opção: \n'),
    read(Option),
    manage_concession_stand_option(Option).

manage_concession_stand_option(1) :-
    add_item,
    manage_concession_stand.
manage_concession_stand_option(2) :-
    list_items,
    manage_concession_stand.
manage_concession_stand_option(3) :-
    edit_item,
    manage_concession_stand.
manage_concession_stand_option(4) :-
    remove_item,
    manage_concession_stand.
manage_concession_stand_option(5) :-
    true.
manage_concession_stand_option(_) :-
    write('Opção inválida. Tente novamente.'), nl,
    manage_concession_stand.

% Função auxiliar para ler um arquivo
read_file_1(Stream, Items) :-
    read_line(Stream, Line),
    (   Line = end_of_file
    ->  Items = []
    ;   Items = [Line|Items1],
        read_file_1(Stream, Items1)
    ).