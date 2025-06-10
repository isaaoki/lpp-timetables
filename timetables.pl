/** Sistema de Organizacao de Escalas com Restricoes

Este projeto organiza turnos em um cronograma, considerando diferentes tipos de restricao 
como a disponibilidade, a preferencia e as compatibilidades entre as pessoas envolvidas. 

Autores:
- Isabela 
- Lucas

*/

%% dias_semana(-Dias:list)
dias_semana([seg, ter, qua, qui, sex]).

%% horario(-Horario:int) 
horario(8).
horario(10).
horario(14).

%% turno(+Quantidade:int, +Horario:int)
% Representa um turno com a quantidade de pessoas necessaria e horario
turno(1, 8).
turno(2, 10).
turno(1, 14).

% ---------------------------------------------
% FATOS E REGRAS DA DISPONIBILIDADE DAS PESSOAS
% ---------------------------------------------

%% disponivel_a_partir(+Pessoa:atom, +Valor:int|atom)
% Define a partir de que dia/horario a pessoa esta disponivel
disponivel_a_partir(tinos, 10).
disponivel_a_partir(joca, seg).
disponivel_a_partir(michele, ter).
disponivel_a_partir(michele, 8).

%% disponivel_ate(+Pessoa:atom, +Valor:int|atom) 
% Define ate que dia/horario a pessoa esta disponivel
disponivel_ate(tinos, 14).
disponivel_ate(joca, qua).
disponivel_ate(michele, qua).
disponivel_ate(isa, 10).

%% disponivel(+Pessoa:atom, +Valor:int|atom) 
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

%% disponivel(+Pessoa:atom, +Horario:int)
% Verdadeiro se pessoa esta disponivel entre intervalo de horarios
disponivel(Pessoa, Horario) :-
	horario(Horario),
	disponivel_a_partir(Pessoa, HorarioInicio),
	horario(HorarioInicio),
	disponivel_ate(Pessoa, HorarioFim),
	horario(HorarioFim),
	Horario > HorarioInicio, 
	Horario < HorarioFim.

%% disponivel(+Pessoa:atom, +Horario:int)
% Verdadeiro se uma pessoa estiver disponivel a partir de um horario (sem horario fim)
disponivel(Pessoa, Horario) :-
    \+ disponivel_ate(Pessoa, horario(_)),
    disponivel_a_partir(Pessoa, HorarioInicio),
    horario(HorarioInicio),
    horario(Horario),
    Horario >= HorarioInicio.

%% disponivel(+Pessoa:atom, +Dia:atom)
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

%% disponivel_dia_horario(+Pessoa:atom, +Dia:atom, +Horario:int)
% Verdadeiro se pessoa esta disponivel em um dia E horario
disponivel_dia_horario(Pessoa, Dia, Horario) :-
	dias_semana(Dias),
	member(Dia, Dias),
	horario(Horario),
	disponivel(Pessoa, Dia),
	disponivel(Pessoa, Horario).

% -----------------------------------------
% FATOS E REGRAS DA PREFERENCIA DAS PESSOAS
% -----------------------------------------

%% prefere_a_partir(+Pessoa:atom, +Valor:int|atom) 
% Define a partir de que dia/horario a pessoa prefere
prefere_a_partir(tinos, 10).

%% prefere_ate(+Pessoa:atom, +Valor:int|atom)
% Define ate que dia/horario a pessoa prefere
prefere_ate(tinos, 14).

%% prefere(+Pessoa:atom, +Valor:int|atom) 
% Define explicitamente o dia/horario que a pessoa prefere
prefere(michele, ter).
prefere(michele, 10).
prefere(tinos, seg).

%% prefere(+Pessoa:atom, +Horario:int)
% Verdadeiro se pessoa prefere um intervalo de horarios
prefere(Pessoa, Horario) :-
	horario(Horario),
	prefere_a_partir(Pessoa, HorarioInicio),
	horario(HorarioInicio),
	prefere_ate(Pessoa, HorarioFim),
	horario(HorarioFim),
	Horario > HorarioInicio, 
	Horario < HorarioFim.

%% prefere(+Pessoa:atom, +Horario:int)
% Verdadeiro se uma pessoa preferir a partir de um horario (sem horario fim)
prefere(Pessoa, Horario) :-
    \+ prefere_ate(Pessoa, horario(_)),
    prefere_a_partir(Pessoa, HorarioInicio),
    horario(HorarioInicio),
    horario(Horario),
    Horario >= HorarioInicio.

%% prefere(+Pessoa:atom, +Dia:atom)
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

%% prefere_dia_horario(+Pessoa:atom, +Dia:atom, +Horario:int)
% Verdadeiro se pessoa prefere um dia E horario
prefere_dia_horario(Pessoa, Dia, Horario) :-
	dias_semana(Dias),
	member(Dia, Dias),
	horario(Horario),
	prefere(Pessoa, Dia),
	prefere(Pessoa, Horario).

% -------------------------------------
% FATOS DA COMPATIBILIDADE DAS PESSOAS
% -------------------------------------

%% detesta(+Pessoa1:atom, +Pessoa2:atom)
detesta(michele, joca).

% -----------------
% MONTAR CRONOGRAMA
% -----------------

%% cronograma_semana(+Dias:list, -CronogramaSemana:list)
% Gera as diferentes possibilidades do cronograma da semana
% Caso base: não há mais dias, o cronograma é vazio
cronograma_semana([], []).

% Caso 1: gera o cronograma do dia e adiciona a lista de cronogramas
cronograma_semana([Dia | T1], [CronogramaDia | T2]) :-
	cronograma_dia(Dia, CronogramaDia),
	cronograma_semana(T1, T2).

%% cronograma_dia(+Dia:atom, -Cronograma:list)
% Gera o cronograma de um dia, passando por cada horario
cronograma_dia(Dia, Cronograma) :-
	findall(Horario, horario(Horario), Horarios),
	cronograma_horarios(Dia, Horarios, Cronograma).

%% cronograma_horarios(+Dia:atom, +Horarios:list, -Cronograma:list)
% Gera os grupos alocados para cada horario em um determinado dia
% Caso base: não há mais horários, o cronograma é vazio
cronograma_horarios(_, [], []).

% Caso 1: Nao existe grupo possivel ([]) para o horario, adiciona lista vazia
cronograma_horarios(Dia, [Horario | T1], [[] | T2]) :-
	grupos_possiveis(Dia, Horario, Grupos),
	Grupos == [], !,
	cronograma_horarios(Dia, T1, T2).

% Caso 2: Gera os grupos possiveis do horario, escolhe um grupo e continua
cronograma_horarios(Dia, [Horario | T1], [Grupo | T2]) :-
	grupos_possiveis(Dia, Horario, Grupos),
	member(Grupo, Grupos),
	cronograma_horarios(Dia, T1, T2).

%% grupos_possiveis(+Dia:atom, +Horario:int, -Grupos:list)
% Retorna uma lista com os grupos possiveis de pessoas disponiveis em um turno
% Retorna primeiro pessoas que preferem o horario
% Remove grupos sem compatibilidade
grupos_possiveis(Dia, Horario, Grupos) :-
	turno(Quantidade, Horario),

	% Pessoas disponiveis e que preferem aquele dia e horario
	findall(Pessoa, (
		disponivel_dia_horario(Pessoa, Dia, Horario),
		prefere_dia_horario(Pessoa, Dia, Horario)),
	DisponiveisPrefere),
	% Pessoas disponiveis e que nao preferem aquele dia e horario
	findall(Pessoa, (
		disponivel_dia_horario(Pessoa, Dia, Horario),
		\+ prefere_dia_horario(Pessoa, Dia, Horario)),
	DisponiveisNaoPrefere),
	% Junta todos os disponiveis
	append(DisponiveisPrefere, DisponiveisNaoPrefere, Disponiveis),
	length(Disponiveis, QuantidadeDisponiveis),
	
	(	QuantidadeDisponiveis >= Quantidade -> 
		% Gera toda as combinacoes possiveis da lista Disponiveis com a Quantidade do turno, conferindo a compatibilidade
		findall(Grupo, (combinar(Quantidade, Disponiveis, Grupo), checa_compatibilidade(Grupo)), Grupos) ;
		% Se tem menos pessoas disponiveis, usa todas as pessoas, conferindo a compatibilidade
		(checa_compatibilidade(Disponiveis) -> Grupos = [Disponiveis] ; Grupos = [])
	).

%% checa_compatibilidade(+Grupo:list)
% Verdadeiro se nao existe pessoas que detestam outras no grupo
checa_compatibilidade([]).
checa_compatibilidade([Pessoa1 | T]) :-
	\+ (member(Pessoa2, T), detesta(Pessoa1, Pessoa2)),
	\+ (member(Pessoa2, T), detesta(Pessoa2, Pessoa1)),
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

% ----------------------
% MAIN E FUNCIONALIDADES
% ----------------------

%% main/0
% Exibe opcoes do menu. A opcao 3 encerra o programa
main :-
	repeat,
	nl,
	write('---- Sistema de Organizacao de Escalas com Restricoes ----'), nl, 
	write('Opcoes:'), nl, 
	write('1. Montar cronograma da semana'), nl,
	write('2. Montar cronograma por dia'), nl,
	write('3. Sair'), nl,
	write('opcao> '),
	read(Opcao),
	opcao_menu(Opcao),
	(Opcao == 3 -> ! ; fail). 

%% imprimir_todos_cronogramas(+Cronogramas:list, +N:int)
% Itera por uma lista com todos cronogramas, imprime a iteracao N e o cronograma da semana
imprimir_todos_cronogramas([], _).
imprimir_todos_cronogramas([Cronograma | T], N) :-
	format('~n~nCronograma ~w:', [N]),
	dias_semana(Dias),
	imprimir_cronograma(Dias, Cronograma),
	N1 is N + 1,
	imprimir_todos_cronogramas(T, N1).

%% imprimir_cronograma(+Dias:list, +CronogramaDias:list)
% Itera pelos dias e pelo cronograma do dia, imprimindo o dia e seus horarios
imprimir_cronograma([], []).
imprimir_cronograma([Dia | T1], [CronogramaDia | T2]) :-
	format('~nDia: ~w', [Dia]),
	findall(Horario, horario(Horario), Horarios),
	imprimir_horarios(Horarios, CronogramaDia),
	imprimir_cronograma(T1, T2).

%% imprimir_horarios(+Horarios:list, +CronogramaHorarios:list)
% Itera pelos horarios de um dia, imprimindo o horario e as pessoas alocadas
imprimir_horarios([], []).
imprimir_horarios([Horario | T1], [CronogramaHorario | T2]) :-
	format('~n  ~w:00 - ~w', [Horario, CronogramaHorario]),
	imprimir_horarios(T1, T2).

%% opcao_menu(+Opcao:int)
opcao_menu(1) :-
	!,
	dias_semana(Dias),
	findall(CronogramaSemana, cronograma_semana(Dias, CronogramaSemana), Cronogramas),
	imprimir_todos_cronogramas(Cronogramas, 1), nl.

opcao_menu(2) :-
	!,
	dias_semana(Dias),
	format('Dias: ~w~n', [Dias]),
	write('Digite o dia: '),
	read(Dia),
	(member(Dia, Dias) -> 
		cronograma_dia(Dia, CronogramaDia),
		imprimir_cronograma([Dia], [CronogramaDia]) ;
		write('Opcao invalida! Tente novamente...')
	),
	nl.

opcao_menu(3) :-
	!,
	write('Saindo...'), nl.

opcao_menu(_) :-
	write('Opcao invalida! Tente novamente...'), nl.