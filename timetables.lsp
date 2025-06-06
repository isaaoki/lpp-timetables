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

;;; (setf disponiveis ()) ; (nome horario dia)

(setf prefere_relac '(

))

(defun pertencep(item lista) ; retorna T se item pertence a lista
    (if (member item lista :test #'equal) 
        T 
        nil
    )
)

(defun combinacoesHorario (n lista)
    (cond
        ((< n 0) nil)
        ((= n 0) (list nil))
        ((> n (length lista)) nil)
        (T (append 
            (mapcar #'(lambda (x) (cons (first lista) x)) (combinacoesHorario (1- n) (rest lista)))
	        (combinacoesHorario n (rest lista))
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

(defun processDispoRelac(disponivel_temp) (let* ((lista_saida ())) 
    (dolist (relacA disponivel_temp)
        (dolist (relacB disponivel_temp)
            (setf nomeA (car relacA) nomeB (car relacB) infoA (second relacA) infoB (second relacB))
            (if (and (eql nomeA nomeB) (typep infoA (type-of (car DIAS))) (typep infoB (type-of (car HORARIOS)))) ; checa se relacA e relacB tratam-se da mesma pessoa, se relacA refefere-se a um dia e se relalcB a um horario
                (push (list nomeA infoA infoB) lista_saida)
            )
        )
    )
    lista_saida
))

(defun cronograma_horario (dia turno) ; (cronograma_horario 'ter '(1 8)) -> ((MICHELE TER 8) (JOCA TER 10) (VANESSA TER 8))
    (let* ((disponivel_dia_horario '()))
        (loop for disponivel_temp in disponiveis do
            (let* ((dia_disponivel (second disponivel_temp)) (horario_disponivel (third disponivel_temp)))
                    (if (and (eql dia_disponivel dia) (eql horario_disponivel (second turno))) ; pessoa está disponível nesse dia e turno
                    (push (list (car disponivel_temp) dia_disponivel horario_disponivel)
                      disponivel_dia_horario)
                    ) 
            )
        )
    disponivel_dia_horario) ; retorna a lista de pessoas disponiveis em um dia e horario (uma lista)
)

(defun cronograma_dia (dia) ; (cronograma_dia 'ter) -> (NIL ((MICHELE TER 10) (JOCA TER 10) (VANESSA TER 10)) ((MICHELE TER 8) (VANESSA TER 8)))
    (let* ((disponiveis_dia '()))
        (loop for turno in turnos do 
            (setf disponiveis_dia (enqueue (cronograma_horario dia turno) disponiveis_dia))
        )
    disponiveis_dia) ; retorna a lista de pessoas disponiveis em um dia (lista de listas)
)

(defun cronograma_semana() ; (cronograma_semana) -> ((((TINOS SEX 14)) ((TINOS SEX 10)) NIL) (((BARA QUI 14)) ((VANESSA QUI 10)) ((VANESSA QUI 8) (MIRELA QUI 8))) (((TINOS QUA 14)) ((MICHELE QUA 10) (JOCA QUA 10) (TINOS QUA 10)) ((MICHELE QUA 8))) (NIL ((MICHELE TER 10) (JOCA TER 10) (VANESSA TER 10)) ((MICHELE TER 8) (VANESSA TER 8))) (((TINOS SEG 14)) ((JOCA SEG 10) (TINOS SEG 10)) NIL))
    (let* ((disponiveis_semana '()))
        (loop for dia in DIAS do
            (setf disponiveis_semana (enqueue (cronograma_dia dia) disponiveis_semana))
        ) 
    disponiveis_semana) ; retorna a lista de pessoas disponiveis em uma semana (lista de listas de listas)
)

(defun nthHorario(pos) (nth pos HORARIOS)) ; (nthhorarios 2) -> 14

(defun posDiaSem(dia) (position dia DIAS)) ; (nthDiaSem 'ter) -> 1

(defun enqueue(val lis) (append lis (list val)))

(defun main()
    (let* ()
        (setf disponivel_temp (append DISPONIVEL (processAPartirAte DISPONIVEL_A_PARTIR DISPONIVEL_ATE)))
        (setf disponiveis (processDispoRelac disponivel_temp))
    )
)
