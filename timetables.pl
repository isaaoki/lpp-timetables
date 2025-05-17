% FATOS DAS DATAS E HORARIOS 
dia(seg).
dia(ter).
dia(qua).
dia(qui).
dia(sex).

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

% disponivel(Pessoa, Dia) :-
% 	dia(Dia),
% 	disponivel_a_partir(Pessoa, DiaInicio),
% 	dia(DiaInicio),
% 	disponivel_ate(Pessoa, DiaFim),
% 	dia(DiaFim),
%   findall(DiaSemana, dia(DiaSemana), Dias), 
% Questão: como achar a posição do dia => achar dias intermediários

disponivel_a_partir(tinos, 10).
disponivel_a_partir(joca, seg).
disponivel_a_partir(michele, seg).
disponivel_a_partir(michele, 8).

disponivel_ate(joca, qua).

% Preferencia de horários e dias

% Relações de gostar