;; Carrega os outros ficheiros de código, escreve e lê ficheiros, e trata da interação com o utilizador.
;; Autores: Nuno Martinho & João Coelho.

#|
    Testes
        Tabuleiro A - 3 caixas
        Tabuleiro B - 7 caixas
        Tabuleiro C - 10 caixas
        Tabuleiro D - 10 caixas
        Tabuleiro E - 20 caixas
        Tabuleiro F - 35 caixas
|#

;; ============ CARREGAR FICHEIROS ============

(load "procura.lisp")
(load "puzzle.lisp")


;; ============= INPUT/OUTPUT =============

;; (ler-tabuleiros)
(defun ler-tabuleiros ()
"Le os tabuleiros no ficheiro problemas.dat"
    (with-open-file (stream "problemas.dat" :if-does-not-exist nil)
        (do ((result nil (cons next result))
                (next (read stream nil 'eof) (read stream nil 'eof)))
                    ((equal next 'eof) (reverse result))
        )
    )
)

;; (print-tabuleiro (ler-tabuleiros))
(defun print-tabuleiro (tabuleiro &optional (stream t))
"Imprime um tabuleiro do ficheiro problemas.dat"
    (not (null (mapcar #'(lambda (l)
        (format stream "~%~t~t ~a" l)) tabuleiro))
    )
    (format t "~%")
)

;; (print-tabuleiros (ler-tabuleiros))
(defun print-tabuleiros (tabuleiros &optional (stream t))
"Imprime os tabuleiros do ficheiro problemas.dat"
    (not (null (mapcar #'(lambda (tabuleiro)
        (format stream "~%~t~t ~a" (print-tabuleiro tabuleiro))) tabuleiros))
    )
    (format t "~%")
)


;; ============= START =============

(defun iniciar ()
"Inicializa o programa"
    (menu)
    (let ((opcao (read)))
        (if 
            (or (not (numberp opcao)) (< opcao 1) (> opcao 3))
                (progn (format t "Escolha uma opção válida!") (iniciar))
                (ecase opcao
                    ('1 (progn 
                            (let ((tabuleiro (opcao-tabuleiro 'iniciar))) 
                                (if (listp tabuleiro) (print-tabuleiro (second tabuleiro)))
                            )
                            (iniciar)
                        )
                    )
                    ('2 (progn
                            (let ((solucao (opcao-algoritmo)))
                                (progn
                                    (format t "~%Tabuleiro ~a" (first solucao))
                                    (format t "~%  - Algoritmo ~a" (second solucao))
                                    (format t "~%  - Objetivo: ~a" (third solucao))
                                    (format t "~%  - Solucao:")
                                    (print-tabuleiro (car (fifth solucao)))
                                )
                            )
                            (iniciar)
                        )
                    )
                    ('3 (progn (format t "Obrigado por jogar!~%~%") (quit)))
                )
        )
    )
)

;; ============= MENUS =============

(defun menu ()
"Mostra o menu inicial"
    (progn
        (format t "~%A carregar jogo...~%")
        (sleep 1)
        (format t "~%o                              o")
        (format t "~%|      - Dots and Boxes -      |")
        (format t "~%|                              |")
        (format t "~%|   1 - Visualizar problemas   |")
        (format t "~%|   2 - Resolver um problema   |")
        (format t "~%|   3 - Sair                   |")
        (format t "~%o                              o")
        (format t "~%~%>> ")
    )
)


(defun tabuleiros-menu (&optional (i 1) (problemas (ler-tabuleiros)))
"Mostra os tabuleiros disponíveis no menu"
    (cond ((null problemas) 
            (progn
                (format t "~%|                                |")
                (format t "~%|        0 - Voltar atras        |") 
                (format t "~%o                                o")
                (format t "~%~%>> ")
            )
        )
        (T (progn
                (if (= i 1) 
                    (progn 
                        (format t "~%o                                o")
                        (format t "~%|    - Escolha o tabuleiro: -    |")
                        (format t "~%|                                |")
                    )
                )
                (format t "~%|        ~a - Tabuleiro ~a         |" i (code-char (+ i 64)))
                (tabuleiros-menu (+ i 1) (cdr problemas))
            )
        )
    )
)

(defun objetivo-menu ()
"Mostra uma mensagem para escolher o numero de caixas fechadas"
    (progn
        (format t "~%o                                                o")
        (format t "~%|     - Defina o numero de caixas fechadas -     |")
        (format t "~%|                                                |")
        (format t "~%|                  0 - Voltar                    |")
        (format t "~%o                                                o")
        (format t "~%~%>> ")
    )
)


(defun algoritmos-menu ()
"Mostra os algoritmos disponiveis no menu"
    (progn
        (format t "~%o                                   o")
        (format t "~%|      - Escolha o algoritmo -      |")
        (format t "~%|                                   |")
        (format t "~%|         1 - Breadth-First         |")
        (format t "~%|          2 - Depth-First          |")
        (format t "~%|              3 - A*               |")
        (format t "~%|                                   |")
        (format t "~%|            0 - Voltar             |")
        (format t "~%o                                   o")
        (format t "~%~%>> ")
    )
)

(defun profundidade-menu ()
"Mostra uma mensagem para escolher a profundidade"
    (progn
        (format t "~%o                                                o")
        (format t "~%|        - Defina a profundidade maxima -        |")
        (format t "~%|                 - a utilizar -                 |")
        (format t "~%|                                                |")
        (format t "~%|                  0 - Voltar                    |")
        (format t "~%o                                                o")
        (format t "~%~%>> ")
    )
)

(defun heuristica-menu ()
"Mostra uma mensagem para escolher a profundidade"
    (progn
        (format t "~%o                                                o")
        (format t "~%|       - Defina a heuristica a utilizar -       |")
        (format t "~%|                                                |")
        (format t "~%|           1 - Heuristica Enunciado             |")
        (format t "~%|         2 - Heuristica Personalizada           |")
        (format t "~%|                                                |")
        (format t "~%|                  0 - Voltar                    |")
        (format t "~%o                                                o")
        (format t "~%~%>> ")
    )
)

;; ============= MENU OPÇÕES =============

(defun opcao-tabuleiro (&optional (voltar 'iniciar))
"Recebe um tabuleiro do menu"
    (progn 
        (tabuleiros-menu)
        (let ((opcao (read)))
            (cond ((equal opcao '0) (funcall voltar))
                  ((not (numberp opcao)) (progn (format t "Escolha uma opção válida~%")))
                  (T
                    (let ((lista (ler-tabuleiros)))
                        (if (or (< opcao 0) (> opcao (length lista)))
                            (progn 
                                (format t "Escolha uma opcao valida!") (opcao-tabuleiro 'tabuleiros-menu)
                            )
                            (list opcao (nth (1- opcao) lista))
                        )
                    )
                  )
            )
        )
    )
)

(defun opcao-objetivo ()
"Recebe um valor de caixas fechadas do utilizador"
    (progn 
        (objetivo-menu)
        (let ((opcao (read)))
            (cond ((equal opcao '0) (opcao-tabuleiro 'opcao-algoritmo))
                  ((or (not (numberp opcao)) (< opcao 0))
                    (progn
                        (format t "Escolha uma opcao valida!~%")
                        (opcao-objetivo)
                    )
                  )
                  (T opcao)
            )
        )
    )
)

;; FUNCAO INACABADA - FALTAM METER AS FUNCOES DOS ALGORITMOS
;; <solucao>::= (<id-tabuleiro> <algoritmo> <objetivo> <hora-inicio> <caminho-solucao> <hora-fim> <profundidade>)
(defun opcao-algoritmo ()
"Recebe a opcao de algoritmo do utilizador e executa-o"
    (progn
        (algoritmos-menu)
        (let ((opcao (read)))
            (cond ((equal opcao '0) (iniciar))
                    ((or (< opcao 0) (> opcao 4)) (progn (format t "Escolha uma opcao valida!~%") (opcao-algoritmo)))
                    ((not (numberp opcao)) (progn (format t "Escolha uma opcao valida!~%")))
                    (T (let* (
                                (no-tabuleiro (opcao-tabuleiro 'opcao-algoritmo))
                                (objetivo (opcao-objetivo))
                                (id-tabuleiro (code-char (+ (first no-tabuleiro) 64)))
                                (tabuleiro (second no-tabuleiro))
                                (no (list (criar-no tabuleiro nil objetivo)))
                            )
                        (ecase opcao
                            (1
                                (let ((solucao (list id-tabuleiro 'BFS objetivo (hora-atual) (bfs 'expandir-no no) (hora-atual))))
                                    (progn 
                                        (ficheiro-estatisticas solucao) 
                                        solucao
                                    )
                                )
                            )
                            (2
                                (let* (
                                        (profundidade (opcao-profundidade))
                                        (solucao (list id-tabuleiro 'DFS objetivo (hora-atual) (dfs 'expandir-no profundidade no) (hora-atual) profundidade))
                                    )
                                    (progn
                                        (ficheiro-estatisticas solucao)
                                        solucao
                                    )
                                )
                            )
                            (3
                                (let* (
                                        (heuristica (opcao-heuristica))
                                        (solucao (list id-tabuleiro 'A* objetivo (hora-atual) (a* 'expandir-no-a* heuristica no) (hora-atual)))
                                    )
                                    (progn
                                        (ficheiro-estatisticas solucao)
                                        solucao
                                    )
                                )
                            )
                        )
                    ))
            )
        )
    )
)

(defun opcao-profundidade ()
"Recebe um valor de profundidade maxima do utilizador"
    (if (not (profundidade-menu))
        (let ((opcao (read)))
            (cond ((equal opcao '0) (opcao-objetivo))
                  ((or (not (numberp opcao)) (< opcao 0))
                    (progn
                        (format t "Escolha uma opcao valida!~%")
                        (opcao-profundidade 'profundidade-menu)
                    )
                  )
                  (T opcao)
            )
        )
    )
)

(defun opcao-heuristica ()
"Recebe um valor que cooresponde a heuristica escolhida pelo utilizador"
    (if (not (heuristica-menu))
        (let ((opcao (read)))
            (cond ((equal opcao '0) (opcao-objetivo))
                  ((or (not (numberp opcao)) (< opcao 0) (> opcao 2))
                    (progn
                        (format t "Escolha uma opcao valida!~%")
                        (opcao-heuristica 'heuristica-menu)
                    )
                  )
                  (T (ecase opcao
                        (1
                            'heuristica-base
                        )
                        (2
                            'heuristica-top
                        )
                  ))
            )
        )
    )
)

;; ============= ESTATISTICAS =============

;; <solucao>::= (<id-tabuleiro> <algoritmo> <objetivo> <hora-inicio> <caminho-solucao> <profundidade> <hora-fim>)

;;(ficheiro-estatisticas '("solucao" "A" "BFS" (hora-atual) (hora-atual)))
(defun ficheiro-estatisticas (solucao)
"Ficheiro de resultados estatisticos (solucao + dados estatisticos sobre a eficiencia)"
    (let* (
            (id-tabuleiro (first solucao))
            (algoritmo (second solucao))
            (objetivo (third solucao))
            (hora-inicio (fourth solucao))
            (caminho-solucao (fifth solucao))
            (hora-fim (sixth solucao))
            (profundidade (seventh solucao))
           )

        (with-open-file (file "resultados.dat" :direction :output :if-does-not-exist :create :if-exists :append)
            (ecase algoritmo
                ('bfs (estatisticas file id-tabuleiro algoritmo objetivo caminho-solucao hora-inicio hora-fim))
                ('dfs (estatisticas file id-tabuleiro algoritmo objetivo caminho-solucao hora-inicio hora-fim profundidade))
                ('a* (estatisticas file id-tabuleiro algoritmo objetivo caminho-solucao hora-inicio hora-fim))
            )
        )
    )
)

(defun hora-atual ()
"Retorna a hora atual (hh mm ss)"
    (multiple-value-bind (s m h)
            (get-decoded-time)
        (format nil "~a:~a:~a" h m s))
)

(defun estatisticas (stream id-tabuleiro algoritmo objetivo caminho-solucao hora-inicio hora-fim &optional profundidade)
"Solução e dados de eficiência para os algoritmos"
    (progn
        (format stream "~%Tabuleiro ~a" id-tabuleiro)
        (format stream "~% - Algoritmo: ~a" algoritmo)
        (format stream "~% - Objetivo: ~a caixas" objetivo)
        (format stream "~% - Solucao encontrada")
        (print-tabuleiro (no-solucao caminho-solucao) stream)
        (format stream "~% - Fator de ramificacao media: ~f" (fator-ramificacao-media caminho-solucao))
        (if (eql algoritmo 'DFS)
            (format stream "~% - Profundidade maxima: ~a" profundidade)
        )
        (format stream "~% - Nº nos gerados: ~a" (num-nos-gerados caminho-solucao))
        (if (eql algoritmo 'A*)
            (format stream "~% - Nº nos expandidos: ~a" (num-nos-expandidos-a* caminho-solucao))
            (format stream "~% - Nº nos expandidos: ~a" (num-nos-expandidos caminho-solucao))
        )
        (format stream "~% - Penetrancia: ~f" (penetrancia caminho-solucao))
        (format stream "~% - Inicio: ~a" hora-inicio)
        (format stream "~% - Fim: ~a~%~%" hora-fim)
    )
)