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

(setf disponiveis ())

(defun pertence(item lista) ; retorna T se tupla pertence a disponiveis
    (if (member item lista :test #'equal) 
        T 
        nil
    )
)

(defun process_a_partir_ate()
    (loop for relacA in dispo_a_partir do
        (loop for relacB in dispo_ate do
            (if (and (eq (car relacA) (car relacB)) (eql (type-of (second relacA)) (type-of (second relacB))))
                (if (pertence (second relacA) dias)
                    (let* ((adding nil)) (loop for dia in dias do (let* ()
                        (if (and (>= (position dia dias) (position (second relacA) dias)) (<= (position dia dias) (position (second relacB) dias))) (push (list (car relacA) dia) dispo_relac))
                    )))
                    (let* ((adding nil)) (loop for horario in horarios do (let* ()
                        (if (and (>= (position horario horarios) (position (second relacA) horarios)) (<= (position horario horarios) (position (second relacB) horarios))) (push (list (car relacA) horario) dispo_relac))
                    )))
                )
            )
        )
    )
)

(defun addToDisponiveis(dispo_dia dispo_horario disponiveis) ; (addToDisponiveis)
    (loop for relacA in dispo_dia do
        (loop for relacB in dispo_horario do 
            (if (eq (car relacA) (car relacB)) 
                (push (list (car relacA)(second relacB)(second relacA)) disponiveis)
            )
        )
    )   
)

(defun nthHorario(pos) (nth pos horarios)) ; (nthhorarios 2) -> 14

(defun posDiaSem(dia) (position dia dias)) ; (nthDiaSem 'ter) -> 1

(defun disponivelp(tupla) ; retorna T se tupla pertence a disponiveis
    (if (member tupla disponiveis :test #'equal) 
        T 
        nil
    )
)

(defun main()
    (let* ()
        (addToDisponiveis dispo_dia dispo_horario disponiveis)
        (write disponiveis)
    )
)