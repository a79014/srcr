%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

:- op( 900,xfy,'::' ).
:- dynamic utente/4.
:- dynamic servico/4.
:- dynamic consulta/4.
:- dynamic (-)/1.
:- dynamic excecao/1.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% conhecimento perfeito positivo

% utente: #IdUt, Nome, Idade, Cidade 
utente(1 , 'ze'		, 20, 'braga'	  ).
utente(2 , 'maria'	, 55, 'aveiro'	  ).
utente(3 , 'joao'	, 22, 'porto'	  ).
utente(4 , 'rita'	, 52, 'viana'	  ).
utente(5 , 'pedro'	, 32, 'lisboa'	  ).
utente(6 , 'raul'	, 25, 'braga'	  ).
utente(7 , 'manuel'	, 36, 'guimaraes' ).
utente(8 , 'fabio'	, 43, 'aveiro'	  ).

% serviço: #IdServ, Descrição, Instituição, Cidade
servico(1 , 'radiografia'		  , 'hpb'				, 'braga'	).
servico(2 , 'ortopedia'			  , 'hospital de aveiro', 'aveiro'	).
servico(3 , 'radiografia'		  , 'hospital de aveiro', 'aveiro'	).
servico(4 , 'oftalmologia'		  , 'hpb'				, 'braga'	).
servico(5 , 'ortopedia'			  , 'hospital sao joao'	, 'porto'	).
servico(6 , 'pneumologia'		  , 'hpb'				, 'braga'	).
servico(7 , 'oncologia'			  , 'ipo'				, 'porto'	).
servico(8 , 'psiquiatria'		  , 'hpb'				, 'braga'	).
servico(9 , 'otorrinolaringologia', 'hpb'				, 'braga'	).
servico(10, 'otorrinolaringologia', 'hospital sao joao'	, 'porto'	).

% consulta: Data, #IdUt, #IdServ, Custo
consulta('2019-03-20', 2, 2, 43	).
consulta('2018-01-01', 1, 1, 100).
consulta('2018-01-02', 1, 3, 20 ).


%------------------------------------------------------------------

% Conhecimento perfeito negativo
-utente(IdUt,Nome,Idade,Cidade) :- 
		nao(utente(IdUt,Nome,Idade,Cidade)),
		nao(excecao(utente(IdUt,Nome,Idade,Cidade))).


-servico(IdServ,Descricao,Instituicao,Cidade):-
		nao(servico(IdServ,Descricao,Instituicao,Cidade)),
		nao(excecao(servico(IdServ,Descricao,Instituicao,Cidade))).

-consulta(Data,IdUt,IdServ,Custo):-
		nao(consulta(Data,IdUt,IdServ,Custo)),
		nao(excecao(consulta(Data,IdUt,IdServ,Custo))).

%	Inserção de Informação Perfeita Negativa

%é falso que utente, com id 9, chamado zéCid, com 77 anos da chamusca
-utente(9 , 'José Cid'	, 77, 'chamusca').

%nao existe o serviço com id 11, oftalmologia, feita na Clinica ... em braga
-servico(11, 'oftalmologia', 'Clinica Doutor Miguel Pinto'	, 'braga'	).

%nao existe a consulta do dia 1 de Março, sobre o utente com id 100,
%id da consulta 103 com custo de 55
-consulta('2018-01-03', 100, 103, 55 ).

%
%
%

%------------ Invariantes sobre o conhecimento ------------------------

% 1º Invariante:Não introduzir conhecimento negativo repetido:

+(-Termo) :: (findall(-Termo,-Termo,S)
			  ,length(S,N)
			  ,N =< 2).


%Ao inserir conhecimento negativo, contradizer o positivo
+(-Termo) :: nao(Termo).

%Ao inserir conhecimento positivo, contradizer o conhecimento negativo
+Termo :: nao(-Termo).

%--------------- Evolução do conhecimento Negativo Perfeito -----------

% Evolução do Utente
evolucaoUtenteNeg(IdUt,Nome,Idade,Cidade) :-
										  evolucao(-utente(IdUt,Nome,Idade,Cidade)).

% Evolução do Serviço
evolucaoServicoNeg(IdServ,Descricao,Instituicao,Cidade) :-
														evolucao(-servico(IdServ,Descricao,Instituicao,Cidade)).

%Evolução da Consulta
evolucaoConsultaNeg(Data,IdUt,IdServ,Custo) :-
											evolucao(-consulta(Data,IdUt,IdServ,Custo)).




%--------------- Regressão do Conhecimento Negativo Perfeito ----------

% Regressão do Utente
regressaoUtenteNeg(IdUt,Nome,Idade,Cidade) :-
										  remove(-utente(IdUt,Nome,Idade,Cidade)).

% Regressão do Serviço
regressaoServicoNeg(IdServ,Descricao,Instituicao,Cidade) :-
														remove(-servico(IdServ,Descricao,Instituicao,Cidade)).

%Regressão da Consulta
regressaoConsultaNeg(Data,IdUt,IdServ,Custo) :-
											remove(-consulta(Data,IdUt,IdServ,Custo)).



% ----------------- Conhecimento Imperfeito Incerto -------------------

% utente: #IdUt, Nome, Idade, Cidade (desconhecida) 
utente(10,'zara',23,cidade_desconhecida).

excecao(utente(IdUt,Nome,Idade,Cidade)):-
		utente(IdUt,Nome,Idade,cidade_desconhecida).



servico(12,desc_desconhecida,'hpb','braga').

excecao(servico(IdServ,Descricao,Instituicao,Cidade)) :-
		servico(IdServ,desc_desconhecida,Instituicao,Cidade).
				
consulta('2019-03-20',3,5,custo_desconhecido).
excecao(consulta(Data,IdUt,IdServ,Custo)) :- 
				consulta(Data,IdUt,IdServ,custo_desconhecido).

% -------------- Evolução do Conhecimento Incerto


%utente(IdUt,Nome,Idade,Cidade)
evolucaoCidadeIncerta(IdUt,Nome,Idade) :-
							evolucao(utente(IdUt,Nome,Idade,cidade_desconhecida)),
							assert((excecao(utente(Id,N,I,C)) :- utente(Id,N,I,cidade_desconhecida))).


%servico(IdServ,Descricao,Instituicao,Cidade)
evolucaoDescricaoIncerta(IdServ,Instituicao,Cidade) :-
							evolucao(servico(IdServ,desc_desconhecida,Instituicao,Cidade)),
							assert((excecao(servico(Id,D,I,C)):-servico(Id,desc_desconhecida,I,C))).

%consulta(Data,IdUt,IdServ,Custo)
evolucaoCustoIncerta(Data,IdUt,IdServ) :-
					evolucao(consulta(Data,IdUt,IdServ,custo_desconhecido)),
					assert((excecao(consulta(D,IdU,IdS,C)):-consulta(D,IdU,IdS,custo_desconhecido))).


% -------------- Regressao do Conhecimento Incerto --------------------
								
regressaoCidadeIncerta(IdUt,Nome,Idade) :-
							involucao(utente(IdUt,Nome,Idade,cidade_desconhecida)),
							retract((excecao(utente(Id,N,I,C)) :- utente(Id,N,I,cidade_desconhecida))).

%servico(IdServ,Descricao,Instituicao,Cidade)
regressaoDescricaoIncerta(IdServ,Instituicao,Cidade) :-
							involucao(servico(IdServ,desc_desconhecida,Instituicao,Cidade)),
							retract((excecao(servico(Id,D,I,C)):-servico(Id,desc_desconhecida,I,C))).

%consulta(Data,IdUt,IdServ,Custo)
regressaoCustoIncerta(Data,IdUt,IdServ) :-
					involucao(consulta(Data,IdUt,IdServ,custo_desconhecido)),
					retract((excecao(consulta(D,IdU,IdS,C)):-consulta(D,IdU,IdS,custo_desconhecido))).

%------------------- Conhecimento Imperfeito Impreciso ----------------

%Não saber ao certo a idade de um utente:(IdUt,Nome,Idade,Cidade)
excecao(utente(11,'filipa',23,'braga')).
excecao(utente(11,'filipa',24,'braga')).

%nao saber ao certo a instituicao:servico(IdServ,Descricao,Instituicao,Cidade)
excecao(servico(13,'radiografia','ipo','porto')).
excecao(servico(13,'radiografia','hospital sao joao','porto')).


%Não saber ao certo o preço de uma consulta: consulta(Data,IdUt,IdServ,Custo)
excecao(consulta('2019-03-25',7,7,50)).
excecao(consulta('2019-03-25',7,7,70)).


%O caso de estar entre um intervalo de valores.
%Por exemplo uma consulta estar entre 150 e 250 euros
excecao(consulta('2019-03-29',8,9,X)) :- X >= 150, X =< 250.


% -------------- Evolução do conhecimento Impreciso 

evolucaoIdadeImprecisa([]).
evolucaoIdadeImprecisa([Termo|T]) :-
						assert(excecao(Termo)),
						evolucaoIdadeImprecisa(T).

evolucaoHospitalImpreciso([]).
evolucaoHospitalImpreciso([Termo|T]) :-
						assert(excecao(Termo)),
						evolucaoHospitalImpreciso(T).


evolucaoPrecoImpreciso([]).
evolucaoPrecoImpreciso([Termo|T]) :-
						assert(excecao(Termo)),
						evolucaoPrecoImpreciso(T).

% --- Para o Caso de ter o preço entre um conjunto:
%Recebe como arg o custo inferior(Ci) e o custo Superior(Cs)
evolucaoPrecoContinuoImpreciso(D,IdU,IdS,Ci,Cs) :-
				assert((excecao(consulta(D,IdU,IdS,X)):- X >= Ci, X =< Cs)).

% ------------- Regressao do conhecimento Impreciso

regressaoExcecao([]).
regressaoExcecao([Termo|R]) :- remove(excecao(Termo)),
							   regressaoExcecao(R).	

%-------------- Conhecimento Interdito ----------
/*
	queremos "esconder" o valor real das variaveis,
	ou seja, nao vamos permitir ninguem aceder aos seus valores.
*/							   


% Primeiro -> Por exemplo, um utente é uma celebridade e pretende esconder
% a sua idade

%utente(IdUt,Nome,Idade,Cidade)
utente(12,'manuel goucha',idade_interdita,'lisboa').
nulo(idade_interdita).

excecao(utente(Id,N,I,C)) :-
			utente(Id,N,idade_interdita,C).


%% Invariante que não permita inserir dados sobre o que tem
% valores interditos

+utente(IdUt,Nome,Idade,Cidade) :: (
							findall(I,(utente(12,Nome,I,Cidade),nao(nulo(I))),S),
							length(S,N),
							N==0).			


%------------ Evolução do conhecimento Interdito ----------------------

/*
	Passos:
		1º: Evolucao do termo
		2º: Inserir os invariantes
		3º: Inserir por excepçao
		4º: Insercao do valor nulo
*/
%utente(IdUt,Nome,Idade,Cidade)
evolucaoIdadeInterdita(I,N,C):-
			evolucao(utente(I,N,idade_interdita,C)),
			assert(+utente(IdUt,Nome,Idade,Cidade) ::
			(findall(Ida,(utente(I,Nome,Ida,Cidade),nao(nulo(Ida))),S),
			length(S,LT),
			LT==0)),
			assert((excecao(utente(IdUt,Nome,Ida,Cidade)) :- utente(IdUt,Nome,idade_interdita,Cidade))),
			assert(nulo(idade_interdita)).


%regressao da idade interdita
regressaoIdadeInterdita(I,N,C):-
		involucao(utente(I,N,idade_interdita,C)),
		remove(+utente(IdUt,Nome,Idade,Cidade) :: 
			(findall(IDA,(utente(I,Nome,IDA,Cidade),nao(nulo(IDA))),S),
				length(S,COMP),
				COMP==0)),
				remove((excecao(IdUt,Nome,IDA,Cidade):-
					utente(IdUt,Nome,idade_interdita,Cidade))),
					remove(nulo(idade_interdita)).


/*
	Primeiro caso: Utente com cidade desconhecida


		% utente: #IdUt, Nome, Idade, Cidade 
		utente(10,'zara',23,cidade_desconhecida).
*/

verificaCidadeIncerta(utente(IdU, Nome, Idade, Cidade))
     :- removeCidadeIncerta(utente(IdU, Nome, Idade,Cidade)).

%funcao que remove a cidade

removeCidadeIncerta(utente(IdU, Nome, Idade, Cidade))
     :- demo(utente(IdU, Nome, Idade, cidade_desconhecida),
     verdadeiro),
     remove(utente(IdU, Nome, Idade, cidade_desconhecida)),
     retract((excecao(utente(Id, N, I, C)) :-
     utente(Id, N, I, cidade_desconhecida))).


/*
	servico(12,desc_desconhecida,'hpb','braga').
	% serviço: #IdServ, Descrição, Instituição, Cidade
*/

verificaDescricaoIncerta(servico(IdS, Descricao, Instituicao, Cidade))
     :- removeDescricaoIncerta(servico(IdS, Descricao, Instituicao,Cidade)).

removeDescricaoIncerta(servico(IdS, Descricao, Instituicao, Cidade))
     :- demo(servico(IdS, desc_desconhecida, Instituicao, Cidade),
     verdadeiro),
     remove(servico(IdS, desc_desconhecida, Instituicao, Cidade)),
     retract((excecao(servico(Id, D, I, C)) :-
     servico(Id, desc_desconhecida, I,C))).


/*
	consulta('2019-03-20',3,5,custo_desconhecido).
	consulta(Data,IdUt,IdServ,Custo)

*/     
verificaCustoIncerta(consulta(Data,IdUt,IdServ,Custo))
     :- removeDescricaoIncerta(consulta(Data,IdUt,IdServ,Custo)).

 removeCustoIncerta(consulta(Data, IdUt, IdServ, Custo))
     :- demo(consulta(Data, IdUt, IdServ, custo_desconhecido),
     verdadeiro),
     remove(consulta(Data, IdUt, IdServ, custo_desconhecido)),
     retract((excecao(consulta(D, IDU, IDS, C)) :-
     consulta(D, IDU, IDS, custo_desconhecido))).    



%-------------- Conhecimento Imperfeito Impreciso ---------------------

/*
	excecao(utente(11,'filipa',23,'braga')).

	Temos a idade repetida
*/


apagaExcecoesIdade(utente(IdU, Nome, Idade, Cidade)) :-
                    demo(excecao(utente(IdU, Nome, _,Cidade)), verdadeiro),
                    findall(utente(Id, N, I, M),
     				excecao(utente(IdU,N,_,M)),S),
                    regressaoExcecao(S).
apagaExcecoesIdade(utente(IdU, Nome, Idade, Cidade)). 


/*
nao saber ao certo a instituicao:servico(IdServ,Descricao,Instituicao,Cidade)
	excecao(servico(13,'radiografia','ipo','porto')).
	excecao(servico(13,'radiografia','hospital sao joao','porto')).

*/

apagaExcecoesInstituicao(servico(IdServ, Descricao, Instituicao, Cidade)) :-
                    demo(excecao(servico(IdServ, Descricao, _,Cidade)), verdadeiro),
                    findall(servico(Id, D, I, C),
     				excecao(servico(IdServ,D,_,C)),S),
                    regressaoExcecao(S).
apagaExcecoesInstituicao(servico(IdServ, Descricao, Instituicao, Cidade)).

/*
	Termos o custo repetido

%Não saber ao certo o preço de uma consulta: consulta(Data,IdUt,IdServ,Custo)
excecao(consulta('2019-03-25',7,7,50)).
excecao(consulta('2019-03-25',7,7,70)).

*/

apagaExcecoesCusto(consulta(Data, IdUt, IdServ, Custo)) :-
                    demo(excecao(consulta(Data, IdUt, IdServ,_)), verdadeiro),
                    findall(consulta(D, IDU, IDS, C),
     				excecao(consulta(Data,IDU,IDS,_)),S),
                    regressaoExcecao(S).
apagaExcecoesCusto(consulta(Data, IdUt, IdServ, Custo)). /*Não entendo pq esta linha no fim*/



%----------- Sistema de Inferencia ------------------------------------

conjuncao(verdadeiro,verdadeiro,verdadeiro).
conjuncao(verdadeiro,desconhecido,desconhecido).
conjuncao(desconhecido,verdadeiro,desconhecido).
conjuncao(desconhecido,desconhecido,desconhecido).
conjuncao(falso,_,falso).
conjuncao(_,falso,falso).


disjuncao(verdadeiro,_,verdadeiro).
disjuncao(_,verdadeiro,verdadeiro).
disjuncao(falso,falso,falso).
disjuncao(falso,desconhecido,desconhecido).
disjuncao(desconhecido,falso,desconhecido).
disjuncao(desconhecido,desconhecido,desconhecido).


demoC([],R).
demoC([Q],R) :- demo(Q,R).
demoC([Q1,e,Q2|T],R) :-
 	demo(Q1,R1),
  	demoC([Q2|T],R2),
  	conjuncao(R1,R2,R).


demoD([],R).
demoD([Q],R) :- demo(Q,R).	
demoD([Q1,ou,Q2|T],R) :-
 	demo(Q1,R1),	
  	demoD([Q2|T],R2),	
  	disjuncao(R1,R2,R).	

/*
	Temos o demo Lista -> simplesmente vai atravessar e verificar a todos os elems
*/
demoListas([], []).
demoListas([H|T], R) :-
        demoThreeValued(H, A),
        demoListas(T, B),
        R = [A|B].


/*
	Exemplos de execução:
	demoThreeValued((utente(1 , 'ze', 20, 'braga'),e,utente(1 , 'ze', 20, 'braga')),R).
	demoThreeValued((utente(1 , 'ze', 20, 'braga'),e,utente(1 , 'ze', 20, 'Sintra')),R).
	demoThreeValued((utente(1 , 'ze', 20, 'braga'),ou,utente(1 , 'ze', 20, 'braga')),R).
	demoThreeValued((utente(1 , 'ze', 20, 'braga'),ou,utente(1 , 'ze', 20, 'Sintra')),R).


	Demo Listas

	demoListas([],R).
	demoListas([(utente(1 , 'ze', 20, 'braga'),e,utente(1 , 'ze', 20, 'braga')),(utente(1 , 'ze', 20, 'braga'),e,utente(1 , 'ze', 20, 'Sintra')),(utente(1 , 'ze', 20, 'braga'),ou,utente(1 , 'ze', 20, 'Sintra'))],R).
*/

% 1 && 1 = 1
demoThreeValued((Q1,e,Q2), verdadeiro) :- demo(Q1, verdadeiro), demo(Q2,verdadeiro). 


% 0 && 1 = 0
demoThreeValued((Q1,e,Q2), falso) :- demo(Q1, falso), demo(Q2,verdadeiro).

% 1 && 0 = 0
demoThreeValued((Q1,e,Q2), falso) :- demo(Q1, verdadeiro), demo(Q2,falso).

% ? && 0 = 0
demoThreeValued((Q1,e,Q2), falso) :- demo(Q1, desconhecido), demo(Q2,falso).

% 0 && ? = 0
demoThreeValued((Q1,e,Q2), falso) :- demo(Q1, falso), demo(Q2,desconhecido).

% 0 && 0 = 0
demoThreeValued((Q1,e,Q2), falso) :- demo(Q1, falso), demo(Q2,falso).

% ? && ? = ?
demoThreeValued((Q1,e,Q2), desconhecido) :- demo(Q1, desconhecido), demo(Q2,desconhecido).

% ? && 1 = ?
demoThreeValued((Q1,e,Q2), desconhecido) :- demo(Q1, desconhecido), demo(Q2,verdadeiro).

% 1 && ? = ?
demoThreeValued((Q1,e,Q2), desconhecido) :- demo(Q1, verdadeiro), demo(Q2,desconhecido).

% 1 || 1 = 1
demoThreeValued((Q1,ou,Q2), verdadeiro)  :- demo(Q1, verdadeiro), demo(Q2, verdadeiro).

% 1 || ? = 1
demoThreeValued((Q1,ou,Q2), verdadeiro)  :- demo(Q1,verdadeiro), demo(Q2, desconhecido).

% ? || 1 = 1
demoThreeValued((Q1,ou,Q2), verdadeiro)  :- demo(Q1,desconhecido), demo(Q2, verdadeiro).

% 1 || 0 = 1
demoThreeValued((Q1,ou,Q2), verdadeiro)  :- demo(Q1, verdadeiro), demo(Q2, falso).

% 0 || 1 = 1
demoThreeValued((Q1,ou,Q2), verdadeiro)  :- demo(Q1, falso), demo(Q2, verdadeiro).

% 0 || 0 = 0
demoThreeValued((Q1,ou,Q2), falso)       :- demo(Q1, falso), demo(Q2, falso).

% ? || ? = ?
demoThreeValued((Q1,ou,Q2), desconhecido):- demo(Q1, desconhecido), demo(Q2, desconhecido).

% 0 || ? = ?
demoThreeValued((Q1,ou,Q2), desconhecido):- demo(Q1, falso), demo(Q2, desconhecido).

% ? || 0 = ?
demoThreeValued((Q1,ou,Q2), desconhecido):- demo(Q1, desconhecido), demo(Q2, falso).


%----------- Extras ---------------------------------------------

% Extensao do predicado para registar conhecimento
teste([]).
teste([R|L]) :- R, teste(L).

%Inserção
insere(P) :- assert(P).
insere(P) :- retract(P),!,fail. 

%Evolução
evolucao(Termo) :- findall(Inv,+Termo::Inv,S),
				   insere(Termo),
				   teste(S).

% Extensao do predicado para remover conhecimento
remove(P) :- retract(P).
remove(P) :- assert(P),!,fail.

%
involucao( Termo ) :-  Termo,
					   findall(Inv,-Termo::Inv,S),
					   remove(Termo),
					   teste(S).


% Negação
nao(Questao) :-
    Questao, !, fail.
nao(Questao).

% Usado para determinar se algo é verdade,falos ou desconhecido
% Extensao do predicado demo: questao,resultado -> {V,F,D}
demo( Questao,verdadeiro ) :- Questao.
demo( Questao, falso ) :- -Questao.
demo( Questao,desconhecido ) :- nao( Questao ), nao( -Questao ).

/*
	Tenho de ver isto, decerteza que nao vai funcionar!!!
*/
involucao( Termo ) :-  Termo,
					   findall(Inv,-Termo::Inv,S),
					   remove(Termo),
					   teste(S). 


evolucao(Termo) :- findall(Inv,+Termo::Inv,S),
				   insere(Termo),
				   teste(S).



