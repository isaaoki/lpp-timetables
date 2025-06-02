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

(setf dispo_dia '(
    (tinos seg)
    (tinos qua)
    (tinos sex)
    (bara qui)
    (vanessa ter)
    (vanessa qui)
    (mirela qui)
))

(setf dispo_horario '(
    (bara 14)
    (joca 10)
    (vanessa 8)
    (vanessa 10)
    (mirela 8)
))

(setf disponiveis ())

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
