:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

:- op( 900,xfy,'::' ).
:- dynamic partida/4.
:- dynamic chegada/4.

% Negação
nao(Questao) :-
    Questao, !, fail.
nao(Questao).

% Usado para determinar se algo é verdade,falos ou desconhecido
% Extensao do predicado demo: questao,resultado -> {V,F,D}
demo( Questao,verdadeiro ) :- Questao.
demo( Questao, falso ) :- -Questao.
demo( Questao,desconhecido ) :- nao( Questao ), nao( -Questao ).

-partida(A,B,C,D) :- nao(partida(A,B,C,D)), nao(excepcao(partida(A,B,C,D))).
-chegada(A,B,C,D) :- nao(chegada(A,B,C,D)), nao(excepcao(chegada(A,B,C,D))).

%extensao do predicado partida(Origem, Destino, Hora, Minuto) -> {V,F}
partida(guimaraes, porto, 16, 15).
partida(guimaraes, porto, 21, 45).
excepcao(partida(guimaraes, trofa, 18, 45)).
excepcao(partida(guimaraes, fafe, 18, 45)).
excepcao(partida(guimaraes, braga, 18, 45)).

%extensao do predicado chegada(Origem, Destino, Hora, Minuto) -> {V,F}
chegada(porto, guimaraes, 16, 00).
%incerto
excepcao(chegada(O,D,H,M)) :- chegada(O,D,atrasado,M).
excepcao(chegada(O,D,H,M)) :- chegada(O,D,H,atrasado).
chegada(guimaraes, trofa, atrasado, atrasado).
excepcao(chegada(O,D,H,M)) :- chegada(O,cancelado,H,M).
chegada(braga, cancelado, 19, 00).
%impreciso
excepcao(chegada(trofa, guimaraes,17,M)) :- M >= 00, M =< 30.
%interdito
excepcao(partida(O,D,H,M)) :- partida(#, D, H, M).
excepcao(chegada(O,D,H,M)) :- chegada(#, D, H, M).
partida(#,braga, 21, 00).
chegada(#,braga, 21, 00).
nulo(#).


violento(foinasede).
+livro(Titulo,Autor,Id) :: (solucoes(titulo, violento(titulo), Lista), comprimento(Lista,N), N == 0).

-livro(Titulo, Autor, Id) :: (solucoes(Id, lancamento(D,M,Autor,Id), Lista), comprimento(Lista,N), N==0).



%EVOLUCAO E INVARIANTES

evolucao( Termo ) :-
    solucoes( Invariante,+Termo::Invariante,Lista ),
    insercao( Termo ),
    teste( Lista ).

insercao( Termo ) :-
    assert( Termo ).
insercao( Termo ) :-
    retract( Termo ),!,fail.

teste( [] ).
teste( [R|LR] ) :-
    R,
    teste( LR ).

solucoes(A,B,C) :- findall(A,B,C).


+partida(O,D,H,M) :: (solucoes((D,H,M), (partida(AA,D,H,M), nao(nulo(AA))), S),
length(S,N),
N==0).

