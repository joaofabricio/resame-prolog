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


% predicado que percorre as sublistas do same
% para construir a lista de posicoes do jogo
% que sera usada no predicado todosOsGrupos
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


% predicado que percorre as colunas do same
% para construir a lista de posicoes do jogo
% que sera usada no predicado todosOsGrupos
% predicado utiliza o predicado sublista
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


% cria todos os grupos possiveis para um determinado same 
todosOsGrupos(Same,ListaDeGruposSame) :-
	listaPosicoes(Same,ListaPosicoesSame),
	head(ListaPosicoesSame,Elemento),
	group(Same,Elemento,Group),
	length(Group,TamanhoGrupo),
	TamanhoGrupo > 1,
	deletaElementos(Group,ListaPosicoesSame,NovaListaPosicoesSame),
	append([],[Group],SubListaGrupos),
	todosOsGrupos(Same,NovaListaPosicoesSame,SubListaGrupos,ListaDeGruposSame),!.

todosOsGrupos(Same,ListaPosicoesSame,SubListaGrupos,ListaDeGruposSame) :-
	head(ListaPosicoesSame,Elemento),
 	group(Same,Elemento,Group),
	length(Group,TamanhoGrupo),
	TamanhoGrupo > 1,
	append(SubListaGrupos,[Group],NovaSubListaGrupos),
 	deletaElementos(Group,ListaPosicoesSame,NovaListaPosicoesSame),
 	todosOsGrupos(Same,NovaListaPosicoesSame,NovaSubListaGrupos,ListaDeGruposSame).

todosOsGrupos(Same,ListaPosicoesSame,SubListaGrupos,ListaDeGruposSame) :-
	head(ListaPosicoesSame,Elemento),
 	\+group(Same,Elemento,_),
	delete(ListaPosicoesSame,Elemento,NovaListaPosicoesSame),
	todosOsGrupos(Same,NovaListaPosicoesSame,SubListaGrupos,ListaDeGruposSame).
 
todosOsGrupos(_,[],SubListaGrupos,ListaDeGruposSame) :- 
	%write('   FIM LISTA POSICOES  '),!.
	ListaDeGruposSame = SubListaGrupos,!.


% deleta elementos da lista de posicoes
% deleta as posicoes que pertencem a um grupo
deletaElementos(Group,ListaPosicoesSame,NovaListaPosicoesSame) :-
	head(Group,Elemento),
	delete(ListaPosicoesSame,Elemento,NovaLista), % NovaLista e a lista de posicoes sem o elemento
	tail(Group,CaudaGrupo),
	deletaElementos(CaudaGrupo,NovaLista,NovaListaPosicoesSame),!.

deletaElementos([],ListaPosicoesSame, NovaListaPosicoesSame) :-
	NovaListaPosicoesSame = ListaPosicoesSame,!.

% predicado que verifica a cor (valor)
% DO elemento Pos		
cor(Same, Pos, Cor) :-
   	pos(X, Y) = Pos,
   	nth0(Y, Same, Column),
   	nth0(X, Column, Cor),
   	Cor > 0.

% predicado para buscar os vizinhos
% de uma determinada posicao
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
   	removeGrupo(Same, Group, NewSameWithZeros),
   	removeZeros(NewSameWithZeros, NewSameWithBlanks),
   	remove_blank(NewSameWithBlanks, NewSame).
   	%sort(Group, GSorted),
   	%remove_column(Same, GSorted, 0, [], NewSameWithBlanks),
   	%remove_blank(NewSameWithBlanks, NewSame).

% remove zeros lista same
removeZeros([], []).

removeZeros([F|R], [New|NewSame]) :-
   	delete(F, 0, New),
   	removeZeros(R, NewSame).

% remove espacos em branco coluna same
remove_blank([], []).
remove_blank([[]|R], T) :- !,remove_blank(R, T).
remove_blank([F|R], [F|T]) :- 
   	remove_blank(R, T).


% funcao utilizada para remover o primeiro elemento da lista
remove_1_elemento(Elemento,[Elemento|Cauda],Cauda).

remove_1_elemento(Elemento,[Elemento1|Cauda],[Elemento1|Cauda1]) :-
	remove_1_elemento(Elemento,Cauda,Cauda1). 

% verdadeiro se ListaResultante e a lista Lista com o elemento Elemento 
% removido da posicao POSICAO 
remove_em(Lista,_,Posicao,_) :-
    	length(Lista,TamanhoLista),
    	Posicao > TamanhoLista - 1,
    	write('falhou'),!.	

remove_em(Lista,Elemento,Posicao,ListaResultante) :-
	Posicao =:= 0,
	remove_1_elemento(Elemento,Lista,Nova),
	%delete(Lista,Elemento,Nova),
	ListaResultante = Nova,!.
    
remove_em(Lista,Elemento,Posicao,ListaResultante) :-
    	Acumulador is 0,
    	remove_em(Lista,Elemento,Acumulador,Posicao,_, ListaResultante),!.

remove_em([Cabeca|Cauda],Elemento,Acumulador,Posicao,Sublista,ListaResultante) :-
    	Acumulador < Posicao,
    	Acumulador1 is Acumulador + 1,
    	append(Sublista,[Cabeca],NovaSublista),
    	remove_em(Cauda,Elemento,Acumulador1,Posicao,NovaSublista,ListaResultante).

remove_em([Cabeca|Cauda],Elemento,Acumulador,Posicao,Sublista,ListaResultante) :-
    	Acumulador =:= Posicao,
    	Elemento = Cabeca,    
%    delete([Cabeca|Cauda],Elemento,Sub),
    	append([],Cauda,Sub),
    	append(Sublista,Sub,ListaFinal),
    	ListaResultante = ListaFinal,!.

% verdadeiro se ListaResultante e a lista Lista com o elemento Elemento 
% inserido na posicao POSICAO 
insere_em(Lista, Elemento,Posicao,ListaResultante) :-
    	length(Lista,TamanhoLista),
   	Posicao > TamanhoLista - 1,
    	append(Lista,[Elemento],NovaLista),
    	ListaResultante = NovaLista,!.
	
insere_em(Lista, Elemento, Posicao, ListaResultante) :-
    	Acumulador is 0,
    	insere_em(Lista, Elemento,Acumulador,Posicao,_,ListaResultante),!.
	
insere_em([Cabeca|Cauda],Elemento,Acumulador,Posicao,Sublista,ListaResultante) :- 
    	Acumulador < Posicao, 
    	append(Sublista,[Cabeca],NovaSubLista),
    	AcumuladorNovo is Acumulador + 1,
    	insere_em(Cauda,Elemento,AcumuladorNovo,Posicao,NovaSubLista,ListaResultante).

insere_em([Cabeca|Cauda], Elemento,Posicao,Acumulador, SubLista,ListaResultante) :-
    	Acumulador =:= Posicao,
    	append(_,[Elemento],Sub),
    	append(SubLista,Sub,SubListaFinal),
    	append(SubListaFinal,[Cabeca|Cauda],ListaFinal),
    	ListaResultante = ListaFinal,!. 


% predicado remove cada posicao 
% presente na lista de grupos do same
% Same - jogo
% [pos(X,Y)|Resto] grupo same
% NewSame - novo same formado apos remocao do grupo
removeGrupo(Same,[pos(X,Y)|Resto],NewSame) :-
	nth0(Y,Same,Coluna),
	length(Coluna,TamanhoColuna),
	TamanhoColuna > 1,
	remove_em(Same,Coluna,Y,Novo),
	nth0(X,Coluna,Elemento),
	remove_em(Coluna,Elemento,X, NovaColuna),
	insere_em(NovaColuna,0,X,Nova),
	insere_em(Novo,Nova,Y,SameAlterado),
	removeGrupo(SameAlterado,Resto,NewSame),!.

removeGrupo(SameAlterado, [pos(X,Y)|Resto], NewSame) :-
	nth0(Y,SameAlterado,Coluna),
	length(Coluna,TamanhoColuna),
	TamanhoColuna > 1,
	remove_em(SameAlterado,Coluna,Y,Novo),
	nth0(X,Coluna,Elemento),
	remove_em(Coluna,Elemento,X, NovaColuna),
	insere_em(Novo,NovaColuna,Y,NovoSame),
 	removeGrupo(NovoSame,Resto,NewSame).


removeGrupo(SameAlterado,[pos(_,Y)|_], _) :-
	nth0(Y,SameAlterado,Coluna),
	length(Coluna,TamanhoColuna),
	TamanhoColuna =:= 1,
	remove_em(SameAlterado,Coluna,Y,_),		
	nth0(0,Coluna,Elemento),	
	remove_em(Coluna,Elemento,0, NovaColuna),
	insere_em(SameAlterado,NovaColuna,Y,_).

removeGrupo(SameTemp,[],NewSame) :-
	NewSame = SameTemp,!.
