/** <module> Sistema de Organizacao de Turnos com Restricoes

Este projeto organiza turnos em um cronograma, considerando diferentes tipos de restricao 
como a disponibilidade, a preferencia e as compatibilidades entre as pessoas envolvidas. 

Autores:
- Isabela 
- Lucas

*/

%% dias_semana(-Dias:list) is det
dias_semana([seg, ter, qua, qui, sex]).

%% horario(-Horario:int) is nondet
horario(8).
horario(10).
horario(14).

%% turno(+Quantidade:int, +Horario:int, +Funcao:atom) is nondet
% Representa um turno com a quantidade de pessoas necessaria, horario e funcao
turno(1, 8, professor).
turno(2, 10, professor).
turno(1, 14, professor).

% ---------------------------------------------
% FATOS E REGRAS DA DISPONIBILIDADE DAS PESSOAS
% ---------------------------------------------

%% disponivel_a_partir(+Pessoa:atom, +Valor:int|atom) is nondet
% Define a partir de que dia/horario a pessoa esta disponivel
disponivel_a_partir(tinos, 10).
disponivel_a_partir(joca, seg).
disponivel_a_partir(michele, ter).
disponivel_a_partir(michele, 8).

%% disponivel_ate(+Pessoa:atom, +Valor:int|atom) is nondet
% Define ate que dia/horario a pessoa esta disponivel
disponivel_ate(tinos, 14).
disponivel_ate(joca, qua).
disponivel_ate(michele, qua).

%% disponivel(+Pessoa:atom, +Valor:int|atom) is nondet
% Define explicitamente o dia/horario que a pessoa esta disponivel
disponivel(tinos, seg).
disponivel(tinos, qua).
disponivel(tinos, sex).
disponivel(bara, qui).
disponivel(bara, 14).
disponivel(joca, 10).
disponivel(vanessa, 8).
disponivel(vanessa, 10).
disponivel(vanessa, ter).
disponivel(vanessa, qui).
disponivel(mirela, qui).
disponivel(mirela, 8).

%% disponivel(+Pessoa:atom, +Horario:int) is nondet
% Verdadeiro se pessoa esta disponivel entre intervalo de horarios
disponivel(Pessoa, Horario) :-
	horario(Horario),
	disponivel_a_partir(Pessoa, HorarioInicio),
	horario(HorarioInicio),
	disponivel_ate(Pessoa, HorarioFim),
	horario(HorarioFim),
	Horario > HorarioInicio, 
	Horario < HorarioFim.

%% disponivel(+Pessoa:atom, +Horario:int) is nondet
% Verdadeiro se uma pessoa estiver disponivel a partir de um horario (sem horario fim)
disponivel(Pessoa, Horario) :-
    \+ disponivel_ate(Pessoa, horario(_)),
    disponivel_a_partir(Pessoa, HorarioInicio),
    horario(HorarioInicio),
    horario(Horario),
    Horario >= HorarioInicio.

%% disponivel(+Pessoa:atom, +Dia:atom) is nondet
% Verdadeiro se pessoa esta disponivel entre intervalo de dias
disponivel(Pessoa, Dia) :-
	dias_semana(Dias), 
	nth1(N, Dias, Dia),
	disponivel_a_partir(Pessoa, DiaInicio),
	nth1(NInicio, Dias, DiaInicio),
	disponivel_ate(Pessoa, DiaFim),
	nth1(NFim, Dias, DiaFim),
	N >= NInicio,
	N =< NFim.

%% disponivel_dia_horario(+Pessoa:atom, +Dia:atom, +Horario:int) is semidet 
% Verdadeiro se pessoa esta disponivel em um dia e horario
disponivel_dia_horario(Pessoa, Dia, Horario) :-
	dias_semana(Dias),
	member(Dia, Dias),
	horario(Horario),
	disponivel(Pessoa, Dia),
	disponivel(Pessoa, Horario).

% -----------------------------------------
% FATOS E REGRAS DA PREFERENCIA DAS PESSOAS
% -----------------------------------------

%% prefere_a_partir(+Pessoa:atom, +Valor:int|atom) is nondet
% Define a partir de que dia/horario a pessoa prefere
prefere_a_partir(tinos, 10).

%% prefere_ate(+Pessoa:atom, +Valor:int|atom) is nondet
% Define ate que dia/horario a pessoa prefere
prefere_ate(tinos, 14).

%% prefere(+Pessoa:atom, +Valor:int|atom) is nondet
% Define explicitamente o dia/horario que a pessoa prefere
prefere(michele, ter).
prefere(michele, 10).
prefere(tinos, seg).

%% prefere(+Pessoa:atom, +Horario:int) is nondet
% Verdadeiro se pessoa prefere um intervalo de horarios
prefere(Pessoa, Horario) :-
	horario(Horario),
	prefere_a_partir(Pessoa, HorarioInicio),
	horario(HorarioInicio),
	prefere_ate(Pessoa, HorarioFim),
	horario(HorarioFim),
	Horario > HorarioInicio, 
	Horario < HorarioFim.

%% prefere(+Pessoa:atom, +Horario:int) is nondet
% Verdadeiro se uma pessoa preferir a partir de um horario (sem horario fim)
prefere(Pessoa, Horario) :-
    \+ prefere_ate(Pessoa, horario(_)),
    prefere_a_partir(Pessoa, HorarioInicio),
    horario(HorarioInicio),
    horario(Horario),
    Horario >= HorarioInicio.

%% prefere(+Pessoa:atom, +Dia:atom) is nondet
% Verdadeiro se pessoa prefere um intervalo de dias
prefere(Pessoa, Dia) :-
	dias_semana(Dias), 
	nth1(N, Dias, Dia),
	prefere_a_partir(Pessoa, DiaInicio),
	nth1(NInicio, Dias, DiaInicio),
	prefere_ate(Pessoa, DiaFim),
	nth1(NFim, Dias, DiaFim),
	N >= NInicio,
	N =< NFim.

%% prefere_dia_horario(+Pessoa:atom, +Dia:atom, +Horario:int) is semidet
% Verdadeiro se pessoa prefere um dia e horario
prefere_dia_horario(Pessoa, Dia, Horario) :-
	dias_semana(Dias),
	member(Dia, Dias),
	horario(Horario),
	prefere(Pessoa, Dia),
	prefere(Pessoa, Horario).

% -------------------------------------
% FATOS DA COMPATIBILIDADE DAS PESSOAS
% -------------------------------------

%% detesta(+Pessoa1:atom, +Pessoa2:atom) is nondet
detesta(michele, joca).

% -----------------
% MONTAR CRONOGRAMA
% -----------------

%% cronograma_dia(+Dia:atom, -Cronograma:list) is nondet
% Gera o cronograma de um dia, passando por cada horario
cronograma_dia(Dia, Cronograma) :-
	findall(Horario, horario(Horario), Horarios),
	cronograma_horarios(Dia, Horarios, Cronograma).

%% cronograma_horarios(+Dia:atom, +Horarios:list, -Cronograma:list) is nondet
% Gera os grupos alocados para cada horario em um determinado dia
% Caso base: não há mais horários, o cronograma é vazio
cronograma_horarios(_, [], []).

% Caso 1: Nao existe grupo possivel ([]) para o horario, adiciona lista vazia
cronograma_horarios(Dia, [Horario | Resto], [[] | RestoGrupos]) :-
	grupos_possiveis(Dia, Horario, Grupos),
	Grupos == [], !,
	cronograma_horarios(Dia, Resto, RestoGrupos).

% Caso 2: Gera os grupos possiveis do horario, escolhe um grupo e continua
cronograma_horarios(Dia, [Horario | Resto], [Grupo | RestoGrupos]) :-
	grupos_possiveis(Dia, Horario, Grupos),
	member(Grupo, Grupos),
	cronograma_horarios(Dia, Resto, RestoGrupos).

%% grupos_possiveis(+Dia:atom, +Horario:int, -Grupos:list) is det
% Retorna uma lista com os grupos possiveis de pessoas disponiveis em um turno
% Retorna primeiro pessoas que preferem o horario
% Remove grupos sem compatibilidade
grupos_possiveis(Dia, Horario, Grupos) :-
	turno(Quantidade, Horario, Funcao),

	% Pessoas disponiveis e que preferem aquele dia e horario
	findall((Horario, Pessoa, Funcao), (
		disponivel_dia_horario(Pessoa, Dia, Horario),
		prefere_dia_horario(Pessoa, Dia, Horario)),
	DisponiveisPrefere),
	% Pessoas disponiveis e que nao preferem aquele dia e horario
	findall((Horario, Pessoa, Funcao), (
		disponivel_dia_horario(Pessoa, Dia, Horario),
		\+ prefere_dia_horario(Pessoa, Dia, Horario)),
	DisponiveisNaoPrefere),
	% Junta todos os disponiveis
	append(DisponiveisPrefere, DisponiveisNaoPrefere, Disponiveis),

	% Gera toda as combinacoes possiveis da lista Disponiveis com a quantidade do turno, conferindo a compatibilidade
	findall(Grupo, (combinar(Quantidade, Disponiveis, Grupo), checa_compatibilidade(Grupo)), Grupos).

%% checa_compatibilidade(+Grupo:list) is semidet
% Verdadeiro se nao existe pessoas que detestam outras no grupo
checa_compatibilidade([]).
checa_compatibilidade([(_, Pessoa1, _) | T]) :-
	\+ (member((_, Pessoa2, _), T), detesta(Pessoa1, Pessoa2)),
	\+ (member((_, Pessoa2, _), T), detesta(Pessoa2, Pessoa1)),
	checa_compatibilidade(T).

%% combinar(+Quantidade:int, +Lista:list, -Combinacoes:list)
% Gera combinacoes dos elementos da Lista com uma Quantidade
% Caso base: lista vazia resulta na lista vazia
combinar(0, _, []).

% Caso 1: inclui o primeiro elemento na combinacao, encontra K-1 elementos da cauda
combinar(K, [X | T1], [X | T2]) :-
	K > 0, 
	K1 is K - 1,
	combinar(K1, T1, T2).
% Caso 2: ignora o primeiro elemento da lista, seleciona K elementos da cauda
combinar(K, [_ | T1], T2) :-
	K > 0,
	combinar(K, T1, T2).

% % TESTE
% % Para testar: cria lista com os dias/horarios disponiveis de uma pessoa
% dias_disponiveis(Pessoa, DiasDisponiveis) :-
% 	dias_semana(Dias),
% 	findall(Dia, (
% 		member(Dia, Dias),
% 		disponivel(Pessoa, Dia)
% 	), DiasDisponiveis).

% horarios_disponiveis(Pessoa, HorariosDisponiveis) :-	
% 	findall(Horario, (
% 		horario(Horario),
% 		disponivel(Pessoa, Horario)
% 	), HorariosDisponiveis).

% dias_horarios_disponiveis(Dia, Horario, Disponibilidade) :-
% 	findall(Pessoa, (
% 		disponivel_dia_horario(Pessoa, Dia, Horario)
% 	), Disponibilidade).


% % Para testar: cria lista com os dias disponiveis de uma pessoa
% dias_preferencia(Pessoa, DiasPreferencia) :-
% 	dias_semana(Dias),
% 	findall(Dia, (
% 		member(Dia, Dias),
% 		prefere(Pessoa, Dia)
% 	), DiasPreferencia).

% horarios_preferencia(Pessoa, HorariosPreferencia) :-	
% 	findall(Horario, (
% 		horario(Horario),
% 		prefere(Pessoa, Horario)
% 	), HorariosPreferencia).

% dias_horarios_preferencia(Dia, Horario, Preferencia) :-
% 	findall(Pessoa, (
% 		prefere_dia_horario(Pessoa, Dia, Horario)
% 	), Preferencia).