% FATOS DAS DATAS E HORARIOS 
dias_semana([seg, ter, qua, qui, sex]).

horario(8).
horario(10).
horario(14).

% turno(Quantidade, Horario, Funcao)
turno(1, 8, professor).
turno(1, 10, professor).
turno(1, 14, professor).

% FATOS E REGRAS DAS DISPONIBILIDADES DAS PESSOAS
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
disponivel(milela, 8).

% REGRAS DE DISPONIBILIDADE
disponivel(Pessoa, Horario) :-
	horario(Horario),
	disponivel_a_partir(Pessoa, HorarioInicio),
	horario(HorarioInicio),
	disponivel_ate(Pessoa, HorarioFim),
	horario(HorarioFim),
	Horario >= HorarioInicio, 
	Horario < HorarioFim.

disponivel(Pessoa, Horario) :-
    \+ disponivel_ate(Pessoa, horario(_)),
    disponivel_a_partir(Pessoa, HorarioInicio),
    horario(HorarioInicio),
    horario(Horario),
    Horario >= HorarioInicio.

disponivel(Pessoa, Dia) :-
	dias_semana(Dias), 
	nth1(N, Dias, Dia),
	disponivel_a_partir(Pessoa, DiaInicio),
	nth1(NInicio, Dias, DiaInicio),
	disponivel_ate(Pessoa, DiaFim),
	nth1(NFim, Dias, DiaFim),
	N >= NInicio,
	N =< NFim.

disponivel_a_partir(tinos, 10).
disponivel_a_partir(joca, seg).
disponivel_a_partir(michele, seg).
disponivel_a_partir(michele, 8).

disponivel_ate(joca, qua).
disponivel_ate(michele, sex).

disponivel_dia_horario(Pessoa, Dia, Horario) :-
	dias_semana(Dias),
	member(Dia, Dias),
	horario(Horario),
	disponivel(Pessoa, Dia),
	disponivel(Pessoa, Horario).

% Cria lista com os dias disponiveis de uma pessoa
dias_disponiveis(Pessoa, DiasDisponiveis) :-
	dias_semana(Dias),
	findall(Dia, (
		member(Dia, Dias),
		disponivel(Pessoa, Dia)
	), DiasDisponiveis).

horarios_disponiveis(Pessoa, HorariosDisponiveis) :-	
	findall(Horario, (
		horario(Horario),
		disponivel(Pessoa, Horario)
	), HorariosDisponiveis).

dias_horarios_disponiveis(Pessoa, Disponibilidade) :-
	findall((Dia, Horario), (
		disponivel_dia_horario(Pessoa, Dia, Horario)
	), Disponibilidade).

% REGRAS DE PREFERENCIA
prefere(Pessoa, Horario) :-
	horario(Horario),
	prefere_a_partir(Pessoa, HorarioInicio),
	horario(HorarioInicio),
	prefere_ate(Pessoa, HorarioFim),
	horario(HorarioFim),
	Horario >= HorarioInicio, 
	Horario < HorarioFim.

prefere(Pessoa, Horario) :-
    horario(HorarioFim),
    \+ prefere_ate(Pessoa, HorarioFim),
    prefere_a_partir(Pessoa, HorarioInicio),
    horario(HorarioInicio),
    horario(Horario),
    Horario >= HorarioInicio.

prefere(Pessoa, Dia) :-
	dias_semana(Dias), 
	nth1(N, Dias, Dia),
	prefere_a_partir(Pessoa, DiaInicio),
	nth1(NInicio, Dias, DiaInicio),
	prefere_ate(Pessoa, DiaFim),
	nth1(NFim, Dias, DiaFim),
	N >= NInicio,
	N =< NFim.

% Cria lista com os dias disponiveis de uma pessoa
dias_preferencia(Pessoa, DiasPreferencia) :-
	dias_semana(Dias),
	findall(Dia, (
		member(Dia, Dias),
		prefere(Pessoa, Dia)
	), DiasPreferencia).

% Relações de gostar e não gostar que limitam

% MONTAR CRONOGRAMA