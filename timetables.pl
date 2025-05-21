% FATOS DAS DATAS E HORARIOS 
dia(seg, 1).
dia(ter, 2).
dia(qua, 3).
dia(qui, 4).
dia(sex, 5).

horario(8).
horario(10).
horario(14).

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
    horario(HorarioFim),
    \+ disponivel_ate(Pessoa, HorarioFim),
    disponivel_a_partir(Pessoa, HorarioInicio),
    horario(HorarioInicio),
    horario(Horario),
    Horario >= HorarioInicio.

disponivel(Pessoa, Dia) :-
	dia(Dia, N),
	disponivel_a_partir(Pessoa, DiaInicio),
	dia(DiaInicio, NInicio),
	disponivel_ate(Pessoa, DiaFim),
	dia(DiaFim, NFim),
	N >= NInicio,
	N =< NFim.

disponivel_a_partir(tinos, 10).
disponivel_a_partir(joca, seg).
disponivel_a_partir(michele, seg).
disponivel_a_partir(michele, 8).

disponivel_ate(joca, qua).

% Cria lista com os dias disponiveis de uma pessoa
dias_disponiveis(Pessoa, DiasDisponiveis) :-
	find_all(Dia, disponivel(Pessoa, Dia), DiasDisponiveis).

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
	dia(Dia, N),
	prefere_a_partir(Pessoa, DiaInicio),
	dia(DiaInicio, NInicio),
	prefere_ate(Pessoa, DiaFim),
	dia(DiaFim, NFim),
	N >= NInicio,
	N =< NFim.

% Relações de gostar