(defconstant DIAS '(
    seg ter qua qui sex
))

(defconstant HORARIOS '(
    8 10 14
))

(defconstant TURNOS '(
    (1 8)
    (1 10)
    (1 14)
))

#|
(defconstant PESSOAS '(
    tinos bara joca vanessa mirela michele
))
|#

(defconstant DISPONIVEL '(
    (tinos seg)
    (tinos qua)
    (tinos sex)
    (bara qui)
    (bara 14)
    (joca 10)
    (vanessa 8)
    (vanessa 10)
    (vanessa ter)
    (vanessa qui)
    (mirela qui)
    (mirela 8)
))

(defconstant DISPONIVEL_A_PARTIR '(
    (tinos 10)
    (joca seg)
    (michele ter)
    (michele 8)
))

(defconstant DISPONIVEL_ATE '(
    (tinos 14)
    (joca qua)
    (michele qua)
))

(defconstant PREFERE '(
    (michele ter)
    (michele 10)
    (tinos seg)
))

(defconstant PREFERE_A_PARTIR '(
    (tinos 10)
))

(defconstant PREFERE_ATE '(
    (tinos 14)
))

(defconstant DETESTA '(
    (michele joca)
))
;;; (setf disponiveis ()) ; (nome horario dia)

(defun pertencep(item lista) ; retorna T se item pertence a lista
    (if (member item lista :test #'equal) 
        T 
        nil
    )
)

(defun combinacoes (n lista)
    (cond
        ((< n 0) nil)
        ((= n 0) (list nil))
        ((>= n (length lista)) (list lista))
        (T (append 
            (mapcar #'(lambda (x) (cons (first lista) x)) (combinacoes (1- n) (rest lista)))
	        (combinacoes n (rest lista))
        ))
    )
)

(defun gerarListaAPartirAte (nomeA nomeB infoA infoB) (let* ((lista_saida ())) ; (joca seg joca qua dias horarios) -> ((JOCA QUA) (JOCA TER) (JOCA SEG))
    (if (and (eql nomeA nomeB) (eql (type-of infoA) (type-of infoB))) ; checa se relacA e relacB tratam-se da mesma pessoa e se ambas referem-se ao mesmo tipo de dado.
        (if (pertencep infoA DIAS) ; checa se estamos tratando da adicao de um dia ou de um horario.
            (dolist (dia DIAS) ; caso seja um dia, percorre pela lista dias:
                (if (and (>= (position dia DIAS) (position infoA DIAS)) (<= (position dia DIAS) (position infoB DIAS))) (push (list nomeA dia) lista_saida))
            )
            (dolist (horario HORARIOS) ; caso seja um dia, percorre pela lista dias:
                (if (and (>= (position horario HORARIOS) (position infoA HORARIOS)) (<= (position horario HORARIOS) (position infoB HORARIOS))) (push (list nomeA horario) lista_saida))
            )
        )
    )
    lista_saida
))

(defun gerarListaAPartir (nomeA infoA) (let* ((lista_saida ())) ; (tinos 10 dias horarios) -> ((TINOS 10) (TINOS 14))
    (if (pertencep infoA DIAS) ; checa se estamos tratando da adicao de um dia ou de um horario.
        (dolist (dia DIAS) ; caso seja um dia, percorre pela lista dias:
            (if (>= (position dia DIAS) (position infoA DIAS)) (push (list nomeA dia) lista_saida))
        )
        (dolist (horario HORARIOS) ; caso seja um horario, percorre pela lista horarios:
            (if (>= (position horario HORARIOS) (position infoA HORARIOS)) (push (list nomeA horario) lista_saida))
        )
    )
    lista_saida
))

(defun processAPartirAte(a_partir ate) (let* ((lista_saida ())); (processAPartirAte a_partir ate) -> 
    (dolist (relacA a_partir)
        (setf is_a_partir_only T) ; assumimos, no inicio, que nao ha uma relacao ate compativel.
        (dolist (relacB ate)
            (setf lista (gerarListaAPartirAte (car relacA) (car relacB) (second relacA) (second relacB)))    
            (if lista (setf lista_saida (append lista lista_saida)))
            (if lista (setf is_a_partir_only nil)) ; se a funcao retornou uma lista nao vazia, a mesma foi capaz de achar uma relacao ate compativel com relacA
        )
        ;; caso esteja definida apenas a relacao a_partir:
        (if is_a_partir_only 
            (setf lista_saida (append (gerarListaAPartir (car relacA) (second relacA)) lista_saida))        
        )
    )
    lista_saida
))

(defun processRelac(lista) (let* ((lista_saida ())) 
    (dolist (relacA lista)
        (dolist (relacB lista)
            (setf nomeA (car relacA) nomeB (car relacB) infoA (second relacA) infoB (second relacB))
            (if (and (eql nomeA nomeB) (typep infoA (type-of (car DIAS))) (typep infoB (type-of (car HORARIOS)))) ; checa se relacA e relacB tratam-se da mesma pessoa, se relacA refefere-se a um dia e se relalcB a um horario
                (push (list nomeA infoA infoB) lista_saida)
            )
        )
    )
    lista_saida
))

(defun processDetesta(lista) (let* ((lista_saida ()))
    (dolist (item lista)
        (setf toleravel T)
        (dolist (nomeA item)
            (dolist (nomeB item)
                (if (pertencep (list nomeA nomeB) DETESTA) (setf toleravel nil))
            )
        )
        (if toleravel (push item lista_saida))
    )
    lista_saida
))

(defun combinacoesHorario (turno disponiveis_horario prefere_dia_horario) (let* ((lista_saida ())) ; ((0 (TINOS SEG 10) (JOCA SEG 10)) ((MIRELA SEG 10) (JOCA SEG 10)))
    (setf lista_temp (combinacoes (car turno) disponiveis_horario)) ; monta combinacoes
    (dolist (item_temp lista_temp lista_saida) 
        (setf quant_prefere 0)
        (setf item_saida ())
        (dolist (subitem item_temp)
            (if (pertencep subitem prefere_dia_horario) (incf quant_prefere)) ; se pessoa possui preferencia, incrementa o indice quant_prefere
            (push (car subitem) item_saida)
        )
        (push quant_prefere item_saida)
        (push item_saida lista_saida)
    ) ; ((0 TINOS JOCA) (0 VANESSA MICHELE) ...)
    (processDetesta lista_saida)
))

(defun cronogramaHorario (dia turno disponiveis)
    (let* ((disponivel_dia_horario '()))
        (dolist (disponivel_temp disponiveis disponivel_dia_horario)
            (let* ((dia_disponivel (second disponivel_temp)) (horario_disponivel (third disponivel_temp)))
                    (when (and (eql dia_disponivel dia) (eql horario_disponivel (second turno))) ; pessoa está disponível nesse dia e turno
                    (push (list (car disponivel_temp) dia_disponivel horario_disponivel)
                        disponivel_dia_horario)
                    ) 
            )
        )
        (combinacoesHorario turno disponivel_dia_horario prefere_dia_horario)
    ) ; retorna a lista de pessoas disponiveis em um dia e horario (uma lista)
)

(defun cronogramaDia (dia disponiveis)
    (let* ((disponiveis_dia '()))
        (dolist (turno TURNOS disponiveis_dia) 
            (setf disponiveis_dia (enqueue (cronogramaHorario dia turno DISPONIVEIS) disponiveis_dia))
        )
    ) ; retorna a lista de pessoas disponiveis em um dia (lista de listas)
)

(defun cronogramaSemana(disponiveis)
    (let* ((disponiveis_semana '()))
        (dolist (dia DIAS disponiveis_semana)
            (setf disponiveis_semana (enqueue (cronogramaDia dia disponiveis) disponiveis_semana))
        ) 
    ) ; retorna a lista de pessoas disponiveis em uma semana (lista de listas de listas)
)

(defun nthHorario(pos) (nth pos HORARIOS)) ; (nthhorarios 2) -> 14

(defun posDiaSem(dia) (position dia DIAS)) ; (nthDiaSem 'ter) -> 1

(defun enqueue(val lis) (append lis (list val)))

(defun main()
    (let* ()
        (setf disponivel_temp (append DISPONIVEL (processAPartirAte DISPONIVEL_A_PARTIR DISPONIVEL_ATE)))
        (setf disponiveis (processRelac disponivel_temp))
        (setf prefere_temp (append PREFERE (processAPartirAte PREFERE_A_PARTIR PREFERE_ATE)))
        (setf prefere_dia_horario (processRelac prefere_temp))
        (cronogramaSemana disponiveis)
    )
)