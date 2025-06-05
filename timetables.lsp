(setf dias '(
    seg ter qua qui sex
))

(setf horarios '(
    8 10 14
))

(setf turnos '(
    (1 8)
    (1 10)
    (1 14)
))

#|
(setf pessoas '(
    tinos bara joca vanessa mirela michele
))
|#

(setf disponivel '(
    (tinos seg)
    (tinos qua)
    (tinos sex)
    (bara qui)
    (vanessa ter)
    (vanessa qui)
    (mirela qui)
    (bara 14)
    (joca 10)
    (vanessa 8)
    (vanessa 10)
    (mirela 8)
))

(setf disponivel_a_partir '(
    (tinos 10)
    (joca seg)
    (michele ter)
    (isa ter)
    (michele 8)
))

(setf disponivel_ate '(
    (joca qua)
    (michele qua)
    (michele 10)
))

(setf disponiveis ()) ; (nome horario dia)

(setf prefere_relac '(

))

(defun pertencep(item lista) ; retorna T se item pertence a lista
    (if (member item lista :test #'equal) 
        T 
        nil
    )
)

(defun gerarListaAPartirAte (nomeA nomeB infoA infoB dias hoarios) (let* ((lista_saida ())) ; (joca seg joca qua dias horarios) -> ((JOCA QUA) (JOCA TER) (JOCA SEG))
    (if (and (eql nomeA nomeB) (eql (type-of infoA) (type-of infoB))) ; checa se relacA e relacB tratam-se da mesma pessoa e se ambas referem-se ao mesmo tipo de dado.
        (if (pertencep infoA dias) ; checa se estamos tratando da adicao de um dia ou de um horario.
            (loop for dia in dias do ; caso seja um dia, percorre pela lista dias:
                (if (and (>= (position dia dias) (position infoA dias)) (<= (position dia dias) (position infoB dias))) (push (list nomeA dia) lista_saida))
            )
            (loop for horario in horarios do ; caso seja um horario, percorre pela lista horarios:
                (if (and (>= (position horario horarios) (position infoA horarios)) (<= (position horario horarios) (position infoB horarios))) (push (list nomeA horario) lista_saida))
            )
        )
    )
    lista_saida
))

(defun gerarListaAPartir (nomeA infoA dias hoarios) (let* ((lista_saida ())) ; (tinos 10 dias horarios) -> ((TINOS 10) (TINOS 14))
    (if (pertencep infoA dias) ; checa se estamos tratando da adicao de um dia ou de um horario.
        (loop for dia in dias do ; caso seja um dia, percorre pela lista dias:
            (if (>= (position dia dias) (position infoA dias)) (push (list nomeA dia) lista_saida))
        )
        (loop for horario in horarios do ; caso seja um horario, percorre pela lista horarios:
            (if (>= (position horario horarios) (position infoA horarios)) (push (list nomeA horario) lista_saida))
        )
    )
    lista_saida
))

(defun processAPartirAte(a_partir ate is_disponivel) ; (processAPartirAte disponivel_a_partir disponivel_ate T) -> nil (funcao sem retorno)
    (loop for relacA in a_partir do (let* ((is_a_partir_only T)) ; assumimos, no inicio, que nao ha uma relacao disponivel_ate compativel.
        (loop for relacB in ate do (let* ((lista (gerarListaAPartirAte (car relacA) (car relacB) (second relacA) (second relacB) dias horarios)))
            (if is_disponivel
                (if lista (setf disponivel (append lista disponivel)))
                (if lista (setf prefere_relac (append lista prefere_relac)))
            )
            (if lista (setf is_a_partir_only nil)) ; se a funcao retornou uma lista nao vazia, a mesma foi capaz de achar uma relacao disponivel_ate compativel com relacA
        ))
        ;; caso esteja definida apenas a relacao disponivel_a_partir:
        (if is_a_partir_only (let* ((lista (gerarListaAPartir (car relacA) (second relacA) dias horarios)))
            (if is_disponivel
                (setf disponivel (append lista disponivel))
                (setf prefere_relac (append lista prefere_relac))
            )
        ))
    ))
)

(defun processDispoRelac()
    (loop for relacA in disponivel do
        (loop for relacB in disponivel do (let* ((nomeA (car relacA)) (nomeB (car relacB)) (infoA (second relacA)) (infoB (second relacB)))
            (if (and (eql nomeA nomeB) (typep infoA (type-of (car dias))) (typep infoB (type-of (car horarios)))) ; checa se relacA e relacB tratam-se da mesma pessoa, se relacA refefere-se a um dia e se relalcB a um horario
                (push (list nomeA infoA infoB) disponiveis)
            )
        ))
    )   
)

(defun cronograma_horario (dia turno) ; (cronograma_horario 'ter '(1 8)) -> ((MICHELE TER 8) (JOCA TER 10) (VANESSA TER 8))
    (let* ((disponivel_dia_horario '()))
        (loop for disponivel in disponiveis do
            (let* ((dia_disponivel (second disponivel)) (horario_disponivel (third disponivel)))
                    (if (and (eql dia_disponivel dia) (eql horario_disponivel (second turno))) ; pessoa está disponível nesse dia e turno
                    (push (list (car disponivel) dia_disponivel horario_disponivel)
                      disponivel_dia_horario)
                    ) 
            )
        )
    disponivel_dia_horario) ; retorna a lista de pessoas disponiveis em um dia e horario (uma lista)
)

(defun cronograma_dia (dia) ; (cronograma_dia 'ter) -> (NIL ((MICHELE TER 10) (JOCA TER 10) (VANESSA TER 10)) ((MICHELE TER 8) (VANESSA TER 8)))
    (let* ((disponiveis_dia '()))
        (loop for turno in turnos do 
             (push (cronograma_horario dia turno) disponiveis_dia)
        )
    disponiveis_dia) ; retorna a lista de pessoas disponiveis em um dia (lista de listas)
)

(defun cronograma_semana() ; (cronograma_semana) -> ((((TINOS SEX 14)) ((TINOS SEX 10)) NIL) (((BARA QUI 14)) ((VANESSA QUI 10)) ((VANESSA QUI 8) (MIRELA QUI 8))) (((TINOS QUA 14)) ((MICHELE QUA 10) (JOCA QUA 10) (TINOS QUA 10)) ((MICHELE QUA 8))) (NIL ((MICHELE TER 10) (JOCA TER 10) (VANESSA TER 10)) ((MICHELE TER 8) (VANESSA TER 8))) (((TINOS SEG 14)) ((JOCA SEG 10) (TINOS SEG 10)) NIL))
    (let* ((disponiveis_semana '()))
        (loop for dia in dias do
            (push (cronograma_dia dia) disponiveis_semana)
        ) 
    disponiveis_semana) ; retorna a lista de pessoas disponiveis em uma semana (lista de listas de listas)
)

(defun processPrefereRelac()
    (loop for relacA in prefere_relac do
        (loop for relacB in prefere_relac do
            (if (and 
                    (eql (car relacA) (car relacB))
                    (typep (second relacA) (type-of (car dias)))
                    (typep (second relacB) (type-of (car horarios)))
                )
                (push (list (car relacA) (second relacA) (second relacB)) preferem)
            )
        )
    )
)

; (defun processPrefereAPartir()
;     (loop for relacA in prefere_a_partir do
;         (loop for relacB in prefere_ate do 
;             (if (and (eql(car relacA) (car relacB)))
;                 (if ())
;             )
;         )
;     )

; )


(defun nthHorario(pos) (nth pos horarios)) ; (nthhorarios 2) -> 14

(defun posDiaSem(dia) (position dia dias)) ; (nthDiaSem 'ter) -> 1

(defun main()
    (let* ()
        (processAPartirAte disponivel_a_partir disponivel_ate T)
        (processDispoRelac)
    )
)