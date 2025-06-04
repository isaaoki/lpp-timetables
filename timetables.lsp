(setf dias '(
    seg ter qua qui sex
))

(setf horarios '(
    8 10 14
))

(setf turnos '(
    (8 1 professor)
    (10 1 professor)
    (14 1 professor)
))

#|
(setf pessoas '(
    tinos bara joca vanessa mirela michele
))
|#

(setf dispo_relac '(
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

(setf dispo_a_partir '(
    (tinos 10)
    (joca seg)
    (michele ter)
    (michele 8)
))

(setf dispo_ate '(
    (joca qua)
    (michele qua)
    (michele 10)
))

(setf disponiveis ()) ; (nome horario dia)

(setf prefere_relac '(
    (michele ter)
    (michele 10)
    (tinos seg)
))

(setf prefere_a_partir '(
    (tinos 10)
))

(setf prefere_ate '(
    (tinos 14)
))

(setf preferem ()) 

(defun pertencep(item lista) ; retorna T se item pertence a lista
    (if (member item lista :test #'equal) 
        T 
        nil
    )
)

(defun processAPartirAte()
    (loop for relacA in dispo_a_partir do
        (loop for relacB in dispo_ate do
            (if (and (eql (car relacA) (car relacB)) 
                     (eql (type-of (second relacA)) (type-of (second relacB)))) ; checa se relacA e relacB tratam-se da mesma pessoa e se ambas referem-se ao mesmo tipo de dado
                (if (pertencep (second relacA) dias) ; checa se estamos tratando da adicao de um dia ou de um horario.
                    (let* ((adding nil)) (loop for dia in dias do (let* () ; caso seja um dia, percorre pela lista dias:
                        (if (and (>= (position dia dias) (position (second relacA) dias)) 
                                 (<= (position dia dias) (position (second relacB) dias))) 
                            (push (list (car relacA) dia) dispo_relac)) 
                    )))
                    (let* ((adding nil)) (loop for horario in horarios do (let* () ; caso seja um horario, percorre pela lista horarios:
                        (if (and (>= (position horario horarios) (position (second relacA) horarios)) 
                                 (<= (position horario horarios) (position (second relacB) horarios))) 
                            (push (list (car relacA) horario) dispo_relac))
                    )))
                )
            )
        )
    )
)

(defun processDispoRelac()
    (loop for relacA in dispo_relac do
        (loop for relacB in dispo_relac do 
            (if (and 
                (eql (car relacA) (car relacB)) 
                (typep (second relacA) (type-of (car dias))) 
                (typep (second relacB) (type-of (car horarios)))) ; checa se relacA e relacB tratam-se da mesma pessoa, se relacA refefere-se a um dia e se relacB a um horario
                (push (list (car relacA)(second relacB)(second relacA)) disponiveis) ; adiciona a lista disponiveis
            )
        )
    )   
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
        (addToDisponiveis dispo_dia dispo_horario disponiveis)
        (write disponiveis)
    )
)

; (defun addToDisponiveis(dispo_dia dispo_horario disponiveis) ; (addToDisponiveis)
;     (loop for relacA in dispo_dia do
;         (loop for relacB in dispo_horario do 
;             (if (eq (car relacA) (car relacB)) 
;                 (push (list (car relacA)(second relacB)(second relacA)) disponiveis)
;             )
;         )
;     )   
; )