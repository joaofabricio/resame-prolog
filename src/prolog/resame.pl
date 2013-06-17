% vim: set ft=prolog:

% Neste arquivo estão especificados os predicados que devem ser implementados.
% Você pode criar predicados auxiliares quando necessário.
%
% No arquivo resame_testes.pl estão os testes para alguns predicados.
%
% Para implementar cada predicado, primeiro você deve ler e entender a
% especificação e o teste.
%
% A especificação dos parâmetros dos predicados segue o formato descrito em
% http://www.swi-prolog.org/pldoc/doc_for?object=section%282,%274.1%27,swi%28%27/doc/Manual/preddesc.html%27%29%29
%
% Um Jogo same é representado por uma lista de colunas, sem os elementos nulos
% (zeros).
% Por exemplo, o jogo
% 2 | 3 0 0 0
% 1 | 2 2 2 0
% 0 | 2 3 3 1
% --+--------
%   | 0 1 2 3
% é representado como [[2, 2, 3], [3, 2], [3, 2], [1]].
% O tamanho deste jogo é 3x4 (linhas x colunas).
%
% Uma posição no jogo é representado por uma estrutura pos com dois argumentos
% (lin, col), onde lin é o número da linha e col é o número da coluna.  No
% exemplo anterior, a posição pos(0, 1) tem cor 3, e a posição pos(1, 2) tem
% cor 2.

% Você pode utilizar os predicados definidos no arquivo resame_utils.pl
:- consult(resame_utils).
%% main(+File) is det
%
%  Carrega um jogo same do arquivo File e imprime uma resolução na saída padrão
%  ou sem-solucao se o jogo não tem solução.

main(File) :-
    read_matrix_file(File, Same),
    solve(Same, Moves),
    length(Same, TotalColunas),
    length(Same, TotalLinhas),
    writeMoves(Same, Moves, TotalLinhas, TotalColunas).

main(_) :-
    writeln('sem-solucao').

writeMoves(Same, [pos(X,Y)|Moves], TotalLinhas, TotalColunas) :-!,
    write(X),
    write(' '),
    writeln(Y),
    writeln(''),
    group(Same, pos(X,Y), Group),
    remove_group(Same, Group, NewSame),
    completeWithZeros(NewSame, TotalLinhas, TotalColunas, NewMatrix),
    write_matrix(NewMatrix),
    writeln(''),
    writeMoves(NewSame, Moves, TotalLinhas, TotalColunas).

writeMoves([], [], _, _).

completeWithZeros([], _, 0, []).

completeWithZeros([C|Colunas], TotalLinhas, TotalColunas, [NewLine|NewMatrix]) :-
    TotalColunas > 0,
    writeLinha(C, TotalLinhas, NewLine),
    TotalC is TotalColunas-1,
    completeWithZeros(Colunas, TotalLinhas, TotalC, NewMatrix).

completeWithZeros([], TotalLinhas, TotalColunas, [NewLine|NewMatrix]) :-
    TotalColunas > 0,
    writeLinha([], TotalLinhas, NewLine),
    TotalC is TotalColunas-1,
    completeWithZeros([], TotalLinhas, TotalC, NewMatrix).

writeLinha([], 0, []).

writeLinha([L|Linhas], TotalLinhas, [L|NewLine]) :-
    TotalLinhas > 0,
    TotalL is TotalLinhas-1,
    writeLinha(Linhas, TotalL, NewLine).

writeLinha([], TotalLinhas, [0|NewLine]) :-
    TotalLinhas > 0,
    TotalL is TotalLinhas-1,
    writeLinha([], TotalL, NewLine).

%% solve(+Same, -Moves) is nondet
%
%  Verdadeiro se Moves é uma sequência de jogadas (lista de posições) que
%  quando realizadas ("clicadas") resolvem o jogo Same.
%  Este predicado não tem teste de unidade. Ele é testado pelo testador.

solve([], []).
solve(Same, [M|Moves]) :-
    group(Same, Group),
    remove_group(Same, Group, NewSame),
    [M|_] = Group,
    solve(NewSame, Moves).

%% group(+Same, ?Group) is nondet
%
%  Verdadeiro se Group é um grupo de Same. Group é uma lista de posições
%  (estrutura pos(lin,col)). Este predicado é não determinístico e deve ser
%  capaz de gerar todos os grupos de Same. Este predicado não deve gerar grupos
%  repetidos. Este predicado e group/3 para vão utilizar os mesmos precicados
%  auxiliares.

group(Same, Group) :-
    todosOsGrupos(Same, LGroup),
    member(Group, LGroup).

posicoes(Same, Pos) :-
    posicoes(Same, 0, 0, LPos),
    member(Pos, LPos).
    
posicoes(Same, X, Y, [pos(X,Y)|LPos]) :-
    length(Same, TotalColunas),
    X < TotalColunas,
    nth0(X,Same,Coluna),
    length(Coluna, TotalLinhas),
    Y < TotalLinhas,!,
    NextY is Y+1,
    posicoes(Same, X, NextY, LPos).

posicoes(Same, X, Y, LPos) :-
    length(Same, TotalColunas),
    X < TotalColunas,
    nth0(X,Same,Coluna),
    length(Coluna, TotalLinhas),
    Y >= TotalLinhas,!,
    NextX is X+1,
    posicoes(Same, NextX, 0, LPos).

posicoes(_, _, _, []).

%percorre dentro das colunas - inicio
sublista(Lista,P,ListaFinal) :-
	length(Lista,TamanhoLista),
	Y is 0,
	sublista(TamanhoLista,P,Y,_,ListaFinal),!.

sublista(TamanhoLista,P,Y,SubLista,ListaFinal) :-
	Y < TamanhoLista,
	E = pos(P,Y),
	append(SubLista,[E],Nova),
	NextY is Y + 1,
	sublista(TamanhoLista,P,NextY,Nova,ListaFinal).

sublista(TamanhoLista,_,Y,SubLista,ListaFinal) :-
	Y =:= TamanhoLista,
	ListaFinal = SubLista,!.

%percorre dentro das colunas - fim

%percorre dentro do same - inicio	
listaPosicoes(Same,ListaPosicoesSame) :-
	length(Same,TamanhoSame),
	P is 0,
        listaPosicoes(Same,TamanhoSame,P,_,ListaPosicoesSame),!.

listaPosicoes(Same,TamanhoSame,P,SubLista,ListaPosicoesSame) :-
	P < TamanhoSame,
	nth0(P,Same,ColunaSame),
	sublista(ColunaSame,P,ListaPosicoes),
	append(SubLista,ListaPosicoes,Nova),
	NextP is P + 1,
	listaPosicoes(Same,TamanhoSame,NextP,Nova,ListaPosicoesSame).

listaPosicoes(_,TamanhoSame,P,SubLista,ListaPosicoesSame) :-
	P =:= TamanhoSame,
	ListaPosicoesSame = SubLista,
	length(ListaPosicoesSame,_),!.
	%write('     '),
	%write('Tamanho final:  '),
	%write(TamanhoFinal),!.

%percorre dentro do same - final

%cria todos os grupos possiveis de um determinado same - inicio
todosOsGrupos(Same,ListaDeGruposSame) :-
	listaPosicoes(Same,ListaPosicoesSame),
%	write(ListaPosicoesSame),
	head(ListaPosicoesSame,Elemento),
%	write(' '),
%	write(Elemento),
	group(Same,Elemento,Group),
	length(Group,TamanhoGrupo),
	TamanhoGrupo > 1,
	%writeln(' '),
	%write(' Grupo:  '),
	%write(Group),
	%writeln(' '),
	deletaElementos(Group,ListaPosicoesSame,NovaListaPosicoesSame),
	append([],[Group],SubListaGrupos),
	%write(' Nova lista de posicoes:   '),
	%write(NovaListaPosicoesSame),
	%writeln('  '),
	todosOsGrupos(Same,NovaListaPosicoesSame,SubListaGrupos,ListaDeGruposSame),!.

todosOsGrupos(Same,ListaPosicoesSame,SubListaGrupos,ListaDeGruposSame) :-
	head(ListaPosicoesSame,Elemento),
	%write('  '),
	%write(Elemento),
	%writeln('  '),
 	group(Same,Elemento,Group),
	length(Group,TamanhoGrupo),
	TamanhoGrupo > 1,
	%write('Tamanho Normal :'),
	%write(' '),
	%write(TamanhoGrupo),
	%writeln(' '),
	append(SubListaGrupos,[Group],NovaSubListaGrupos),
	%write(' Grupo;   '),
	%write(Group),
	%writeln('  '),
 	deletaElementos(Group,ListaPosicoesSame,NovaListaPosicoesSame),
	%writeln('Nova Lista de posicoes  '),
	%write(NovaListaPosicoesSame),
	%writeln('  '),
 	todosOsGrupos(Same,NovaListaPosicoesSame,NovaSubListaGrupos,ListaDeGruposSame).

todosOsGrupos(Same,ListaPosicoesSame,SubListaGrupos,ListaDeGruposSame) :-
	head(ListaPosicoesSame,Elemento),
	%write('  TESTE').
	%write('Elemento Posicao: '),
	%write(' '),	
	%write(Elemento),
	%writeln('  '),
 	\+group(Same,Elemento,_),
	%write(' Grupo nao formado '),
	%writeln(' '),
	delete(ListaPosicoesSame,Elemento,NovaListaPosicoesSame),
	%write(NovaListaPosicoesSame),
	%writeln('  '),
	todosOsGrupos(Same,NovaListaPosicoesSame,SubListaGrupos,ListaDeGruposSame).
 
todosOsGrupos(_,[],SubListaGrupos,ListaDeGruposSame) :- 
	%write('   FIM LISTA POSICOES  '),!.
	ListaDeGruposSame = SubListaGrupos,!.
	%write(ListaDeGruposSame),!.
%cria todos os grupos possiveis de um determinado same - fim

% deleta elementos da lista de posicoes
% deleta as posicoes que pertencem a um grupo
deletaElementos(Group,ListaPosicoesSame,NovaListaPosicoesSame) :-
	head(Group,Elemento),
	delete(ListaPosicoesSame,Elemento,NovaLista), % NovaLista e a lista de posicoes sem o elemento
	tail(Group,CaudaGrupo),
	deletaElementos(CaudaGrupo,NovaLista,NovaListaPosicoesSame),!.

deletaElementos([],ListaPosicoesSame, NovaListaPosicoesSame) :-
	NovaListaPosicoesSame = ListaPosicoesSame,!.

		
cor(Same, Pos, Cor) :-
   pos(X, Y) = Pos,
   nth0(Y, Same, Column),
   nth0(X, Column, Cor).

vizinhos(P, V) :-
   P = pos(X, Y),
   Yn is Y+1,
   N = pos(X, Yn),
   Ys is Y-1,
   S = pos(X, Ys),
   Xl is X+1,
   L = pos(Xl, Y),
   Xo is X-1,
   O = pos(Xo, Y),
   V = [N, S, L, O].
   
%% grupo(+Same, +P, -Group) is semidet
%
%  Verdadeiro se Group é um grupo de Same que contém a posição P.
group(Same, P, Group) :-
   vizinhos(P, Vizinhos),
   cor(Same, P, Cor),
   %append(Group, [P], NextGroup),
   %write(NextGroup),
   group(Same, [P], Cor, Vizinhos,Group).


%não tem mais candidatos - condicao base recursao
group(_, ListaFinal, _, [], ListaFinal) :-
	%sort(X,V),
	%write(ListaFinal). 
	length(ListaFinal,TamGroup),
	TamGroup > 1,  
	!.


%F já foi verificado e é membro de Group
group(Same, Group, Cor, [F|R],ListaFinal) :-
   member(F, Group), !, 	
   group(Same, Group, Cor, R,ListaFinal).
   
%group(+Same, ?Group, +Cor, ?Candidatos)   
%F será adicionado à Group
group(Same, Group, Cor, [F|R],ListaFinal) :-
   %validPosition(Same, F),
   cor(Same,F,Cor), !,
   %write(F),
   vizinhos(F, Vizinhos),
   append(R, Vizinhos, ProximosCandidatos),
   append(Group, [F], NextGroup),
   group(Same, NextGroup, Cor, ProximosCandidatos,ListaFinal).

%F não é da mesma cor de Group ou é inválido
group(Same, Group, Cor, [_|R], ListaFinal) :-!,
   group(Same, Group, Cor, R, ListaFinal).


%% remove_group(+Same, +Group, -NewSame) is semidet
%
%  Verdadeiro se NewSame é obtido de Same remov\endo os elemento especificados
%  em Group. A remoção é feita de acordo com as regras do jogo same.
%  Dica:
%    - crie um predicado auxiliar remove_column_group, que remove os elementos
%    de uma coluna específica

%remove_group(Same, Group, NewSame) :-
%    writeln([Same, Group, NewSame]), fail.

remove_group(Same, Group, NewSame) :-
   sort(Group, GSorted),
   remove_column(Same, GSorted, 0, [], NewSameWithBlanks),
   remove_blank(NewSameWithBlanks, NewSame).

%remove column
remove_column([FS|RS], [pos(X, Y)|R], X, Building, NewSame) :- !,
   remove_line(FS, [pos(X, Y)|R], X, 0, [], NewFS, NewGroup),
   append(Building, [NewFS], NextB),
   NextX is X+1,
   remove_column(RS, NewGroup, NextX, NextB, NewSame).

remove_column([FS|RS], Group, X, Building, NewSame) :- !,
   NextX is X+1,
   append(Building, [FS], NextB),
   remove_column(RS, Group, NextX, NextB, NewSame).
   
remove_column([], [], _, NewSame, NewSame):-!.

%remove line
remove_line([_|RL], [pos(X,Y)|R], X, Y, LBuilding, ListReturn, NewGroup) :-!,
   NextY is Y+1,
   remove_line(RL, R, X, NextY, LBuilding, ListReturn, NewGroup).

remove_line([FL|RL], Group, X, Y, LBuilding, ListReturn, NewGroup) :-!,
   NextY is Y+1,
   append(LBuilding, [FL], NextL),
   remove_line(RL, Group, X, NextY, NextL, ListReturn, NewGroup).

remove_line([], NewGroup, _, _, Return, Return, NewGroup):-!.

remove_blank([], []).
remove_blank([[]|R], T) :- !,remove_blank(R, T).
remove_blank([F|R], [F|T]) :- 
   remove_blank(R, T).
