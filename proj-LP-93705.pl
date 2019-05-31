%		Diogo Fouto Silva          

:-[codigo_comum].

%-----------------------------------------------------------------------------
%             aplica_R1_triplo(Triplo, N_Triplo)
%   
%	1_ Verifico qual e' a posicao vazia (ou seja, com variavel);
%   2_ Verifico, de acordo com os restantes membros da lista, 
%       se preencho com 0 ou 1 (com o auxiliar);
%   3_ Preencho.
%
% 	Nota: Se a o conteudo a preencher for nada (ou seja, variavel), 	
%         entao e preenchida com a mesma variavel que ja se encontrava la'
%        (o que facilita a comparacao de puzzles nos predicados mais adiante).
%-----------------------------------------------------------------------------
aplica_R1_triplo([X,Y,Z], [N,Y,Z]) :- 
	var(X), !,
	qual_N([Y,Z], X, N).

aplica_R1_triplo([X,Y,Z], [X,N,Z]) :- 
	var(Y), !,
	qual_N([X,Z], Y, N).

aplica_R1_triplo([X,Y,Z], [X,Y,N]) :- 
	var(Z), !,
	qual_N([X,Y], Z, N).

aplica_R1_triplo([X,Y,Z], _) :- 
	X == Y, X == Z, !,
	false.

aplica_R1_triplo(X,X).

%------------ Auxiliar:

qual_N([P1,P2], X, N) :- 
	P1 == P2 -> N is 1-P1 ; N = X.


%-----------------------------------------------------------------------------
%             aplica_R1_fila_aux(Fila, N_Fila)
%
%	1_ Calcula o tamanho da Fila;
%	2_ Se for 3 ou mais, aplica a R1 aos 3 primeiros elementos da Fila;
%	3_ Guarda o 1 elemento da Fila alterada como Ok1_Head 
%	   e os 2 outros alterados em Ok1_Tail;
%   4_ Junta Ok1_Tail ao resto da Fila inalterada e chama a funcao principal
%	   (recursivamente);
%	5_ Junta tudo e unifica a nova fila com N_Fila.
%
% 	Nota: Se o tamanho da Fila for 2 ou menos, entao N_Fila e' igual a Fila.
% 	Nota: Os aux's retornam a cabeca e a cauda, respetivamente da lista dada.
%----------------------------------------------------------------------------- 
aplica_R1_fila_aux([A,B,C|Rest], N_Fila) :-
	length([A,B,C|Rest], L), L > 2, !,
	aplica_R1_triplo([A,B,C], Ok1),
	gimme_head(Ok1, Ok1_Head),
	gimme_tail(Ok1, Ok1_Tail),
	append(Ok1_Tail, Rest, Dummy),
	aplica_R1_fila_aux(Dummy, Ok2),
	append([Ok1_Head], Ok2, N_Fila).

aplica_R1_fila_aux(Fila, Fila).

%------------ Auxiliar:

gimme_head([Head|_], Head).
gimme_tail([_|Tail], Tail).


%-----------------------------------------------------------------------------
%             aplica_R1_fila(Fila, N_Fila)   
%
%	1_ Aplica o aplica_R1_fila_aux;
%   2_ Se o resultado for igual 'a fila inicial, acaba e retorna essa fila;
%   3_ Se nao, volta a 1_.
%
% 	Nota: Embora a notacao ... -> ... ; ... possa ser desnecessaria/overkill, 
%	      esta ajuda na interpretacao do programa.
%-----------------------------------------------------------------------------      
aplica_R1_fila(Fila, N_Fila) :-
	aplica_R1_fila_aux(Fila, Dummy),
	(Fila == Dummy -> N_Fila = Fila ; aplica_R1_fila(Dummy, N_Fila)).


%-----------------------------------------------------------------------------
%             aplica_R2_fila(Fila, N_Fila)
%
%	1_ Coloca em Only0List os elementos 0 de Fila,
%	   faz o mesmo para Only1List mas com elementos 1;
%	2_ Calcula o tamanho de Fila e das novas listas;
%	3_ Compara os tamanhos das novas listas com metade do tamanho de Fila e, 
%      com a ajuda do auxiliar fillVar, preenche as posicoes vazias com 0 ou 1 
%      ou mantem a variavel.
%
% 	Nota: Se o tamanho de uma das novas listas for maior que metade do tamanho 
%         de Fila, da' erro (false).
%-----------------------------------------------------------------------------
aplica_R2_fila(Fila, N_Fila) :-
	include(sera0, Fila, Only0List),
	include(sera1, Fila, Only1List),
	length(Fila, Len),
	length(Only0List, Len0),
	length(Only1List, Len1),
	(
	  Len0 =:= Len/2 -> fillVar(Fila, 1, N_Fila)
	; Len1 =:= Len/2 -> fillVar(Fila, 0, N_Fila)

	; Len0 > Len/2 -> false
	; Len1 > Len/2 -> false

	; N_Fila = Fila
	).


%------------ Auxiliares:

sera0(X) :- X == 0.
sera1(X) :- X == 1.

fillVar([],_,[]).

fillVar([H1|T1], N, [H2|T2]) :-
	(var(H1) -> H2 = N ; H2 = H1),
	fillVar(T1,N,T2).


%-----------------------------------------------------------------------------
%             aplica_R1_R2_fila(Fila, N_Fila)
%-----------------------------------------------------------------------------
aplica_R1_R2_fila(Fila, N_Fila) :-
	aplica_R1_fila(Fila, Dummy),
	aplica_R2_fila(Dummy, N_Fila).


%-----------------------------------------------------------------------------
%             aplica_R1_R2_puzzle(Puz, N_Puz)
%
%	1_ Com a ajuda do auxiliar, 
%      aplica as regras 1 e 2 a todas as linhas de Puz;
%	2_ Transpoe Puz e volta a 1_, ou seja, aplica as regras 1 e 2 as 'colunas
%
% 	Nota: O auxiliar basicamente chama o aplica_R1_R2_fila varias vezes,
%         aplicando-o a todas as linhas do Puz.
%-----------------------------------------------------------------------------
aplica_R1_R2_puzzle(Puz, N_Puz) :-
	aplica_R1_R2_all_filas(Puz, Temp_Puz),
	mat_transposta(Temp_Puz, Temp_Puz_Transp),
	aplica_R1_R2_all_filas(Temp_Puz_Transp, Puz_Transp),
	mat_transposta(Puz_Transp, N_Puz).

%------------ Auxiliares:

aplica_R1_R2_all_filas([],[]) :- !.

aplica_R1_R2_all_filas([L1|Rest], N_Filas_Puz) :-
	aplica_R1_R2_fila(L1, N_L1),
	aplica_R1_R2_all_filas(Rest, N_Rest),
	append([N_L1], N_Rest, N_Filas_Puz).


%-----------------------------------------------------------------------------
%             inicializa(Puz, N_Puz)
%
%	1_ Aplica o aplica_R1_R2_puzzle;
%   2_ Se o resultado for igual ao puzzle inicial, acaba e retorna esse puzzle;
%   3_ Se nao, volta a 1_.
%
% 	Nota: Embora a notacao ... -> ... ; ... possa ser desnecessaria/overkill, 
%	      esta ajuda na interpretacao do programa.
%-----------------------------------------------------------------------------
inicializa(Puz, N_Puz) :-
	aplica_R1_R2_puzzle(Puz, Dummy),
	(Puz == Dummy -> N_Puz = Puz ; inicializa(Dummy, N_Puz)).


%-----------------------------------------------------------------------------
%             verifica_R3(Puz)
%
%	1_ Verifica a regra 3 apenas nas linhas do Puz, atraves do auxiliar;
%	2_ Transpoe o Puz em Puz_Transp e verifica a regra 3 apenas nas linhas 
%      desse puzzle (O que e' igual a verificar as colunas do Puz).
%-----------------------------------------------------------------------------
verifica_R3(Puz) :-
	verifica_R3_linhas(Puz),
	mat_transposta(Puz, Puz_Transp),
	verifica_R3_linhas(Puz_Transp).

%------------ Auxiliares:

verifica_R3_linhas([_]) :- !.
verifica_R3_linhas([L1,L2|T]) :-
	L1 \== L2,
	verifica_R3_linhas([L2|T]).


%-----------------------------------------------------------------------------
%             propaga_posicoes(Posicoes, Puz, N_Puz)

%	1_ Aplica o aplica_R1_R2_fila 'as linha e coluna da primeira entrada da 	
%	   lista de posicoes a propagar, ou seja, propaga pela 1_vez;

%	2_ Substitui as novas linha e coluna no puzzle e unifica com Puz_Temp;

%	3_ Verifica que novas posicoes foram alteradas ao propagar pela 1_vez,
%	   atraves da funcao auxiliar bag_posicoes_alteradas;

%	4_ Junta a lista dessas novas posicoes alteradas 'a lista principal de
%	   posicoes a propagar exceto a primeira (porque ja' foi propagada em 1_);

%	5_ Recursivamente, propaga o essa nova lista.

% 	NOTA SOBRE AUXILIARES:
%	Os dois primeiros auxiliares devolvem a linha L e coluna C, respetivamente
%	do puzzle dado.
%	O bag_posicoes_alteradas coloca numa lista TODAS as posicoes alteradas 
%	pela propagacao anterior, utilizando o bag_posicoes_lista_alteradas,
%	que apenas encontram as posicoes alteradas duma lista dada.
%-----------------------------------------------------------------------------
propaga_posicoes([], Puz, Puz) :- !.

propaga_posicoes([(L,C)|T], Puz, N_Puz) :-
	gimme_linha_L(L, Puz, Linha_L),
	aplica_R1_R2_fila(Linha_L, N_Linha_L),
	gimme_coluna_C(C, Puz, Coluna_C),
	aplica_R1_R2_fila(Coluna_C, N_Coluna_C),

	mat_muda_linha(Puz, L, N_Linha_L, Puz_Dummy),
	mat_muda_coluna(Puz_Dummy, C, N_Coluna_C, Puz_Temp),
 
	bag_posicoes_alteradas((L,C), Puz, Puz_Temp, New_Pos_Alt),
	append(New_Pos_Alt, T, Pos_Alt),
	propaga_posicoes(Pos_Alt, Puz_Temp, N_Puz).


%------------ Auxiliares:

gimme_linha_L(L, Puz, Linha_L) :- nth1(L, Puz, Linha_L).

gimme_coluna_C(C, Puz, Coluna_C) :-
	mat_transposta(Puz, Puz_Transp),
	nth1(C, Puz_Transp, Coluna_C).

bag_posicoes_alteradas((L,C), Puz, Puz_Temp, New_Pos_Alt) :-
	gimme_linha_L(L, Puz, Linha_L),
	gimme_linha_L(L, Puz_Temp, Linha_L_Temp),

	bag_posicoes_lista_alteradas(L, Linha_L, Linha_L_Temp, New_Pos_Lin, 0, 0),

	gimme_coluna_C(C, Puz, Coluna_C),
	gimme_coluna_C(C, Puz_Temp, Coluna_C_Temp),

	bag_posicoes_lista_alteradas(C, Coluna_C, Coluna_C_Temp, New_Pos_Col, 0, 1),
	append(New_Pos_Lin, New_Pos_Col, New_Pos_Alt).
	

%	Neste predicado e' usado um iterador (Iter) para assinalar a coordenada da
%	posicao alterada.
%	O argumento Lst0Col1 identifica se a lista dada e' coluna ou linha.

bag_posicoes_lista_alteradas(_,[],[],[],_,_) :- !.
bag_posicoes_lista_alteradas(Q, [X|Resto], [Y|Resto2], Bag_Master, Iter, Lst0Col1) :-
	N_Iter is Iter+1,
	(
	 (X \== Y, Lst0Col1 == 0) -> Bag1 = [(Q, N_Iter)]
	;(X \== Y, Lst0Col1 == 1) -> Bag1 = [(N_Iter, Q)] 
	
	;Bag1 = []
	),
	bag_posicoes_lista_alteradas(Q, Resto, Resto2, Bag2, N_Iter, Lst0Col1),
	append(Bag1, Bag2, Bag_Master).


%-----------------------------------------------------------------------------
%             resolve(Puz, Sol)

%	1_ Inicializa Puz como Puz_Temp, verifica a regra 3 em Puz_Temp;
%	2_ Coloca numa lista as posicoes vazias de Puz_Temp, 
%	   com a ajuda do auxiliar find_posicoes_vazias;
%	3_ Tenta preencher as posicoes com 0 ou 1 e propaga, com a ajuda
%	   do auxiliar testar_todas_posicoes_vazias.
%-----------------------------------------------------------------------------
resolve(Puz, Sol) :-
	inicializa(Puz, Puz_Temp),
	verifica_R3(Puz_Temp),
	find_posicoes_vazias(Puz_Temp, Bag, 1),
	testar_todas_posicoes_vazias(Puz_Temp, Bag, Sol).


%------------ Auxiliares:

% 	Neste predicado usamos Itera para assinalar a linha do Puz a ser 
%	investigada para posicoes vazias.

find_posicoes_vazias(Puz, Bag_Master, Itera) :-
	mat_dimensoes(Puz, Num_Lins, Num_Cols),
	Itera =< Num_Lins, !,

%	Verificamos por posicoes vazias da linha Itera.

	find_pos_linha_vazias(Puz, Itera, Num_Cols, 1, Bag1),

	N_Itera is Itera+1,
	find_posicoes_vazias(Puz, Bag2, N_Itera),
	append(Bag1, Bag2, Bag_Master).

find_posicoes_vazias(_,[],_).

%	Neste predicado o Itera assinala a posicao (coluna)
%	da linha a ser analisada.

find_pos_linha_vazias(Puz, L, Num_Cols, Itera, Bag_Master) :-
	Itera =< Num_Cols, !,
	mat_ref(Puz, (L,Itera), Cont),
	(var(Cont) -> Bag1 = [(L,Itera)] ; Bag1 =[]),

	N_Itera is Itera+1,
	find_pos_linha_vazias(Puz, L, Num_Cols, N_Itera, Bag2),
	append(Bag1, Bag2, Bag_Master).

find_pos_linha_vazias(_,_,_,_,[]).

%	Testa cada posicao vazia com 0 ou 1, e faz recursao.

testar_todas_posicoes_vazias(Puz, [], Puz) :- !.

testar_todas_posicoes_vazias(Puz, [Pos|T], N_Puz) :-
	(testar_unica_posicao_vazia(Puz, Pos, 0, Puz_Temp); 
	 testar_unica_posicao_vazia(Puz, Pos, 1, Puz_Temp)), 
	testar_todas_posicoes_vazias(Puz_Temp, T, N_Puz).

testar_unica_posicao_vazia(Puz, Pos, Cont, N_Puz) :-
	mat_muda_posicao(Puz, Pos, Cont, Puz_Temp),
	propaga_posicoes([Pos], Puz_Temp, N_Puz).