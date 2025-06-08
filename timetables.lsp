(defconstant DIAS '(
    seg ter qua qui sex
))

(defconstant HORARIOS '(
    8 10 14
))

(defconstant TURNOS '(
    (1 8)
    (2 10)
    (1 14)
))

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

(defconstant DISPONIVEL-A-PARTIR '(
    (tinos 10)
    (joca seg)
    (michele ter)
    (michele 8)
))

(defconstant DISPONIVEL-ATE '(
    (tinos 14)
    (joca qua)
    (michele qua)
))

(defconstant PREFERE '(
    (michele ter)
    (michele 10)
    (tinos seg)
))

(defconstant PREFERE-A-PARTIR '(
    (tinos 10)
))

(defconstant PREFERE-ATE '(
    (tinos 14)
))

(defconstant DETESTA '(
    (michele joca)
))

; Retorna T se item pertence a lista
(defun pertencep(item lista)
    (if (member item lista :test #'equal) 
        T 
        nil
    )
)

; Gera combinacoes de uma lista a partir de n
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

; Gera lista com pares (nome dia/horario) para informacoes com inicio e fim
; DISPONIVEL-A-PARTIR (tinos seg) e DISPONIVEL-ATE (tinos qua) -> ((TINOS SEG) (TINOS TER) (TINOS QUA))
(defun gerar-lista-a-partir-ate (nomeA nomeB infoA infoB)  
    (let* ((lista-saida ())) 
        ; Checa se trata da mesma pessoa e se ambas referem-se ao mesmo tipo de dado
        (if (and (eql nomeA nomeB) (eql (type-of infoA) (type-of infoB)))
            (if (pertencep infoA DIAS) 
                (dolist (dia DIAS) ; Caso seja um dia, percorre pela lista DIAS
                    (if (and (>= (position dia DIAS) (position infoA DIAS)) (<= (position dia DIAS) (position infoB DIAS))) 
                        (push (list nomeA dia) lista-saida)))

                (dolist (horario HORARIOS) ; Caso seja um horario, percorre pela lista HORARIOS
                    (if (and (>= (position horario HORARIOS) (position infoA HORARIOS)) (<= (position horario HORARIOS) (position infoB HORARIOS))) 
                        (push (list nomeA horario) lista-saida)))))
        lista-saida))

; Gera lista com pares (nome dia/horario) para informacoes com inicio apenas
; DISPONIVEL-A-PARTIR (tinos 10) -> ((TINOS 10) (TINOS 14))
(defun gerar-lista-a-partir (nomeA infoA) 
    (let* ((lista-saida ()))
        (if (pertencep infoA DIAS) 
            (dolist (dia DIAS) ; Caso seja um dia, percorre pela lista DIAS
                (if (>= (position dia DIAS) (position infoA DIAS)) (push (list nomeA dia) lista-saida)))
            
            (dolist (horario HORARIOS) ; Caso seja um horario, percorre pela lista HORARIOS
                (if (>= (position horario HORARIOS) (position infoA HORARIOS)) (push (list nomeA horario) lista-saida))))
        lista-saida))

; Rece listas A-PARTIR e ATE e processa listas para gerar intervalos
(defun process-a-partir-ate(a-partir ate) 
    (let* ((lista-saida ()))
        (dolist (relacA a-partir)
            ; Assumimos, no inicio, que nao ha uma relacao ate compativel
            (let* ((sem-relac-ate T) (nomeA (car relacA)) (infoA (second relacA)))
                (dolist (relacB ate) 
                    (let* ((nomeB (car relacB)) (infoB (second relacB)) (lista (gerar-lista-a-partir-ate nomeA nomeB infoA infoB)))
                        (when lista 
                            ; Se a funcao retornou uma lista nao vazia, a mesma foi capaz de achar uma relacao ate compativel com relacA
                            (setf lista-saida (append lista lista-saida))
                            (setf sem-relac-ate nil))))
                ; Caso esteja definida apenas a relacao a_partir
                (if sem-relac-ate (setf lista-saida (append (gerar-lista-a-partir nomeA infoA) lista-saida)))))
        lista-saida))

; Gera lista com pares (nome dia horario) de acordo com a lista de entrada
(defun process-relac(lista) 
    (let* ((lista-saida ())) 
        (dolist (relacA lista)
            (dolist (relacB lista)
                (let* ((nomeA (car relacA)) (nomeB (car relacB)) (infoA (second relacA)) (infoB (second relacB)))
                ; Checa se relacA e relacB tratam-se da mesma pessoa, se relacA refefere-se a um dia e se relacB a um horario
                    (if (and (eql nomeA nomeB) (typep infoA (type-of (car DIAS))) (typep infoB (type-of (car HORARIOS))))
                        (push (list nomeA infoA infoB) lista-saida)))))
        lista-saida))


; Retorna lista com combinacoes validas, retirando combinacoes com pessoas incompativeis
(defun process-detesta (lista-combinacoes) 
    (let* ((combinacoes-validas ()))
        (dolist (combinacao lista-combinacoes)
            ; Para cada combinacao, verifica se é compativel e ignora o indice de preferencia 
            (let* ((compativel T) (pessoas (cdr combinacao))) 
                ; Compara cada pessoa com o restante das pessoas
                (dolist (pessoaA pessoas)
                    (dolist (pessoaB (cdr (member pessoaA pessoas)))
                        (if (or (pertencep (list pessoaA pessoaB) DETESTA) (pertencep (list pessoaB pessoaA) DETESTA)) 
                            (setf compativel nil))))
                ; Se é uma relacao compativel, adiciona as combinacoes validas
                (if compativel (push combinacao combinacoes-validas))))
        combinacoes-validas))

; Retorna as possiveis combinacoes de um horario, adicionando index indicando a preferencia e processando relacoes detesta
; ((0 (TINOS SEG 10) (JOCA SEG 10)) (1 (MIRELA SEG 10) (JOCA SEG 10)))
(defun montar-combinacoes-horario (turno cronograma-horario prefere-dia-horario) 
    (let* ((lista-saida ()) (lista-combinacoes (combinacoes (car turno) cronograma-horario)))
        (dolist (combinacao lista-combinacoes) 
            ; Para uma combinacao, define a quant-prefere como 0
            (let* ((quant-prefere 0) (item-saida ()))
                (dolist (pessoa combinacao)
                    ; Se pessoa possui preferencia, incrementa o indice quant_prefere e adiciona no item-saida
                    (if (pertencep pessoa prefere-dia-horario) 
                        (incf quant-prefere))
                    (push (car pessoa) item-saida))
                ; Adciona indice ao inicio de item-saida e adiciona item a lista
                (push quant-prefere item-saida)  
                (push item-saida lista-saida)))
        (process-detesta lista-saida)))

; Gera os possiveis cronograma para um dia e um turno
; Retorna lista de combinacoes de pessoas disponiveis e com index prefere em um dia e horario (uma lista)
(defun montar-cronograma-horario (dia turno disponivel-dia-horario prefere-dia-horario)
    (let* ((cronograma-horario '()))
        (dolist (disponibilidade disponivel-dia-horario)
            (let* ((pessoa-disponivel (car disponibilidade)) (dia-disponivel (second disponibilidade)) (horario-disponivel (third disponibilidade)))
                ; Adiciona pessoa se está disponível nesse dia e turno
                (when (and (eql dia-disponivel dia) (eql horario-disponivel (second turno))) 
                    (push (list pessoa-disponivel dia-disponivel horario-disponivel) cronograma-horario))))
        (montar-combinacoes-horario turno cronograma-horario prefere-dia-horario)))


; Gera o cronograma do dia, passando pelos turnos
; Retorna a lista de pessoas disponiveis em um dia (lista de listas)
(defun montar-cronograma-dia (dia disponivel-dia-horario prefere-dia-horario) 
    (let* ((cronograma-dia '()))
        (dolist (turno TURNOS)
            (push (montar-cronograma-horario dia turno disponivel-dia-horario prefere-dia-horario) cronograma-dia))
        (reverse cronograma-dia)))

; Gera o cronograma da semana, passando pelos dias
; Retorna lista de pessoas disponiveis (lista de listas de listas)
(defun montar-cronograma-semana (disponivel-dia-horario prefere-dia-horario) 
    (let* ((cronograma '()))
        (dolist (dia DIAS)
            (push (montar-cronograma-dia dia disponivel-dia-horario prefere-dia-horario) cronograma)) 
        (reverse cronograma)))

; Imprime o cronograma dos horarios do dia fornecido
(defun imprimir-horarios(dia cronograma-dia) (let* ()
    (format  T "Dia: ~w~%" dia)
    (dolist (turno TURNOS)
        (format  T "  ~w:00 - ~w~%" (second turno) (nth (position turno TURNOS) cronograma-dia))
    )
))

; Imprime o cronograma de todos os dias
(defun imprimir-cronograma(cronograma-semana) 
    (dolist (dia DIAS)
        (imprimir-horarios dia (nth (position dia DIAS) cronograma-semana))
    )
)

(defun main()
    (let* ((running T)(opcao 0))
        (setf disponivel-temp (append DISPONIVEL (process-a-partir-ate DISPONIVEL-A-PARTIR DISPONIVEL-ATE)))
        (setf disponivel-dia-horario (process-relac disponivel-temp))
        (setf prefere-temp (append PREFERE (process-a-partir-ate PREFERE-A-PARTIR PREFERE-ATE)))
        (setf prefere-dia-horario (process-relac prefere-temp))

        (loop while running do (cond
            ((eq opcao 0) (let* ()
                (format  T "---- Sistema de Organizacao de Escalas com Restricoes ----~%")
                (format  T "Opcoes:~%")
                (format  T "1. Montar cronograma da semana~%")
                (format  T "2. Montar cronograma por dia~%")
                (format  T "3. Sair~%")
                (format  T "opcao> ")
                (setf opcao (read))
            ))
            ((eq opcao 1) (let* ()
                (imprimir-cronograma (montar-cronograma-semana disponivel-dia-horario prefere-dia-horario))
                (setf opcao 0) ; no final do bloco retorna para o menu de escolha
            ))
            ((eq opcao 2) (let* ((dia))
                (format  T "Dias: ~w~%" DIAS)
                (format  T "Digite o dia: ")
                (setf dia (read))
                (imprimir-horarios dia (montar-cronograma-dia dia disponivel-dia-horario prefere-dia-horario))
                (setf opcao 0) ; no final do bloco retorna para o menu de escolha
            ))
            ((eq opcao 3) (setf running nil))
            ((> opcao 3) (let* ()
                (format  T "Opcao invalida! Tente novamente...~%")
                (setf opcao 0)
            ))
        ))
    )
)