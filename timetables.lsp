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

(defun pertencep(item lista) ; retorna T se item pertence a lista
    (if (member item lista :test #'equal) 
        T 
        nil
    )
)

(defun combinacoes (n lista) ; gera combinacoes de uma lista a partir de n
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

; Gera lista com pares (nome dia/horario) para informacoes com inicio e fim 
(defun gerar-lista-a-partir-ate (nomeA nomeB infoA infoB)  
    (let* ((lista-saida ())) 
        (if (and (eql nomeA nomeB) (eql (type-of infoA) (type-of infoB))) ; checa se trata da mesma pessoa e se ambas referem-se ao mesmo tipo de dado
            (if (pertencep infoA DIAS) ; checa se estamos tratando da adicao de um dia ou de um horario
                (dolist (dia DIAS) ; caso seja um dia, percorre pela lista DIAS
                    (if (and (>= (position dia DIAS) (position infoA DIAS)) (<= (position dia DIAS) (position infoB DIAS))) 
                        (push (list nomeA dia) lista-saida))
                )
                (dolist (horario HORARIOS) ; caso seja um horario, percorre pela lista HORARIOS
                    (if (and (>= (position horario HORARIOS) (position infoA HORARIOS)) (<= (position horario HORARIOS) (position infoB HORARIOS))) 
                        (push (list nomeA horario) lista-saida))
                )
            )
        )
        lista-saida
    )
)

; Gera lista com pares (nome dia/horario) para informacoes com inicio apenas
; DISPONIVEL-A-PARTIR (tinos 10) -> ((TINOS 10) (TINOS 14))
(defun gerar-lista-a-partir (nomeA infoA) (let* ((lista-saida ()))
    (if (pertencep infoA DIAS) ; checa se estamos tratando da adicao de um dia ou de um horario
        (dolist (dia DIAS) ; caso seja um dia, percorre pela lista DIAS
            (if (>= (position dia DIAS) (position infoA DIAS)) (push (list nomeA dia) lista-saida))
        )
        (dolist (horario HORARIOS) ; caso seja um horario, percorre pela lista HORARIOS
            (if (>= (position horario HORARIOS) (position infoA HORARIOS)) (push (list nomeA horario) lista-saida))
        )
    )
    lista-saida
))

(defun process-a-partir-ate(a_partir ate) (let* ((lista_saida ())); (processAPartirAte a_partir ate) -> 
    (dolist (relacA a_partir)
        (setf is_a_partir_only T) ; assumimos, no inicio, que nao ha uma relacao ate compativel.
        (dolist (relacB ate)
            (setf lista (gerar-lista-a-partir-ate (car relacA) (car relacB) (second relacA) (second relacB)))    
            (if lista (setf lista_saida (append lista lista_saida)))
            (if lista (setf is_a_partir_only nil)) ; se a funcao retornou uma lista nao vazia, a mesma foi capaz de achar uma relacao ate compativel com relacA
        )
        ;; caso esteja definida apenas a relacao a_partir:
        (if is_a_partir_only 
            (setf lista_saida (append (gerar-lista-a-partir (car relacA) (second relacA)) lista_saida))        
        )
    )
    lista_saida
))

; Gera lista com pares (nome dia horario) de acordo com a lista de entrada
(defun process-relac(lista) (let* ((lista-saida ())) 
    (dolist (relacA lista)
        (dolist (relacB lista)
            (setf nomeA (car relacA) nomeB (car relacB) infoA (second relacA) infoB (second relacB))
            (if (and (eql nomeA nomeB) (typep infoA (type-of (car DIAS))) (typep infoB (type-of (car HORARIOS)))) ; checa se relacA e relacB tratam-se da mesma pessoa, se relacA refefere-se a um dia e se relalcB a um horario
                (push (list nomeA infoA infoB) lista-saida)
            )
        )
    )
    lista-saida
))

(defun process-detesta (lista-combinacoes) 
    (let* ((combinacoes-validas ())) ; recebe lista e retira combinacoes em que pessoas se detestam
        (dolist (combinacao lista-combinacoes)
            (setf compativel T)
            (dolist (pessoaA combinacao)
                (dolist (pessoaB combinacao)
                    (if (pertencep (list pessoaA pessoaB) DETESTA) (setf compativel nil))
                )
            )
            (if compativel (push combinacao combinacoes-validas))
        ) 
        combinacoes-validas
    )
)

(defun montar-combinacoes-horario (turno cronograma-horario prefere-dia-horario) 
    (let* ((lista-saida ())) ; ((0 (TINOS SEG 10) (JOCA SEG 10)) ((MIRELA SEG 10) (JOCA SEG 10)))
        (setf lista-temp (combinacoes (car turno) cronograma-horario)) ; monta combinacoes
        (dolist (item-temp lista-temp lista-saida) 
            (setf quant-prefere 0)
            (setf item-saida ())
            (dolist (subitem item-temp)
                (if (pertencep subitem prefere-dia-horario) (incf quant-prefere)) ; se pessoa possui preferencia, incrementa o indice quant_prefere
                (push (car subitem) item-saida)
            )
            (push quant-prefere item-saida)
            (push item-saida lista-saida)
        ) ; ((0 TINOS JOCA) (0 VANESSA MICHELE) ...)
        (process-detesta lista-saida)
    )
)

(defun montar-cronograma-horario (dia turno disponivel-dia-horario prefere-dia-horario) ; gera os possiveis cronograma do horario
    (let* ((cronograma-horario '()))
        (dolist (disponivel-temp disponivel-dia-horario cronograma-horario)
            (let* ((dia-disponivel (second disponivel-temp)) (horario-disponivel (third disponivel-temp)))
                    (when (and (eql dia-disponivel dia) (eql horario-disponivel (second turno))) ; checa se pessoa está disponível nesse dia e turno
                    (push (list (car disponivel-temp) dia-disponivel horario-disponivel)
                        cronograma-horario)
                    ) 
            )
        )
        (montar-combinacoes-horario turno cronograma-horario prefere-dia-horario)
    ) ; retorna as possiveis combinacoes de pessoas disponiveis e com index prefere em um dia e horario (uma lista)
)

(defun montar-cronograma-dia (dia disponivel-dia-horario prefere-dia-horario) ; gera o cronograma do dia, passando pelos turnos
    (let* ((cronograma-dia '()))
        (dolist (turno TURNOS cronograma-dia) 
            (setf cronograma-dia (enqueue (montar-cronograma-horario dia turno disponivel-dia-horario prefere-dia-horario) cronograma-dia))
        )
    ) ; retorna a lista de pessoas disponiveis em um dia (lista de listas)
)

(defun montar-cronograma-semana (disponivel-dia-horario prefere-dia-horario) ; gera o cronograma da semana, passando pelos dias
    (let* ((cronograma '()))
        (dolist (dia DIAS cronograma)
            (setf cronograma (enqueue (montar-cronograma-dia dia disponivel-dia-horario prefere-dia-horario) cronograma))
        ) 
    ) ; retorna lista de pessoas disponiveis (lista de listas de listas)
)

; (defun nthHorario(pos) (nth pos HORARIOS)) ; (nthhorarios 2) -> 14

; (defun posDiaSem(dia) (position dia DIAS)) ; (nthDiaSem 'ter) -> 1

(defun enqueue(item lista) (append lista (list item)))

(defun main()
    (let* ()
        (setf disponivel-temp (append DISPONIVEL (process-a-partir-ate DISPONIVEL_A_PARTIR DISPONIVEL_ATE)))
        (setf disponivel-dia-horario (process-relac disponivel-temp))
        (setf prefere-temp (append PREFERE (process-a-partir-ate PREFERE_A_PARTIR PREFERE_ATE)))
        (setf prefere-dia-horario (process-relac prefere-temp))
        (montar-cronograma-semana disponivel-dia-horario prefere-dia-horario)
    )
)