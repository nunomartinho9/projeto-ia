;;  Carrega os outros ficheiros de código, 
;;  escreve e lê ficheiros, e trata da interação com o utilizador
;;  Autores: Nuno Martinho & João Coelho.

;; (bfs-recursivo (third (escolher-problema 1)) (second (escolher-problema 1)) 'usar-operadores 'calcular-pontos 'posicionar-cavalo)
;; ============ CARREGAR FICHEIROS ============

(load "procura.lisp")
(load "puzzle.lisp")

;; ============= INICIAR =============

(defun iniciar ()
"Inicializa o programa"
    (menu)
    (let ((opcao (read)))
        (if 
            (or (not (numberp opcao)) (< opcao 1) (> opcao 3))
                (progn (format t "Escolha uma opção válida!") (iniciar))
                (ecase opcao
                    (1 (progn 
                            (print-tabuleiros)
                            (iniciar)
                        )
                    )
                    (2 (progn
                            (let ((solucao (opcao-algoritmo)))
                                (progn
                                    (format t "~%Tabuleiro ~a" (first solucao))
                                    (format t "~%  - Algoritmo ~a" (second solucao))
                                    (format t "~%  - Solução:")
                                    (if (fifth solucao)
                                        (print-tabuleiro (fifth solucao))
                                        (format t "~%~t~t~t Não existe solução.~%")
                                    )
                                )
                            )
                            (iniciar)
                        )
                    )
                    (3 (progn (format t "Obrigado por jogar!~%~%") (quit)))
                )
        )
    )
)

;; ============= MENUS =============

(defun menu ()
  "Mostra o menu inicial"
    (progn
        ;;(format t "~%A carregar jogo...~%")
        ;;(sleep 1)
        (format t "~%o                                  o")
        (format t "~%|      - Problema do Cavalo -      |")
        (format t "~%|                                  |")
        (format t "~%|   1 - Visualizar problemas       |")
        (format t "~%|   2 - Resolver um problema       |")
        (format t "~%|   3 - Sair                       |")
        (format t "~%o                                  o")
        (format t "~%~%>> ")
    )
)

(defun tabuleiros-menu (&optional (i 1) (problemas (ler-tabuleiros)))
"Mostra os tabuleiros disponíveis no menu"
    (cond ((null problemas) 
            (progn
                (format t "~%|                                |")
                (format t "~%|        0 - Voltar atrás        |") 
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

(defun algoritmos-menu ()
"Mostra os algoritmos disponiveis no menu"
    (progn
        (format t "~%o                                    o")
        (format t "~%|      - Escolha o algoritmo -       |")
        (format t "~%|                                    |")
        (format t "~%|    1 - Breadth-First (iterativo)   |")
        (format t "~%|    2 - Breadth-First (recursivo)   |")
        (format t "~%|    3 - Depth-First                 |")
        (format t "~%|    4 - A*                          |")
        (format t "~%|                                    |")
        (format t "~%|            0 - Voltar              |")
        (format t "~%o                                    o")
        (format t "~%~%>> ")
    )
)

(defun profundidade-menu ()
"Mostra uma mensagem para escolher a profundidade"
    (progn
        (format t "~%o                                                o")
        (format t "~%|        - Defina a profundidade máxima -        |")
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
        (format t "~%|       - Defina a heurística a utilizar -       |")
        (format t "~%|                                                |")
        (format t "~%|           1 - Heurística Enunciado             |")
        (format t "~%|         2 - Heurística Personalizada           |")
        (format t "~%|                                                |")
        (format t "~%|                  0 - Voltar                    |")
        (format t "~%o                                                o")
        (format t "~%~%>> ")
    )
)

;; ============= NAVEGAÇÃO =============

(defun opcao-tabuleiro (&optional (voltar 'iniciar))
"Recebe a opção de um tabuleiro do menu"
    (progn 
        (tabuleiros-menu)
        (let ((opcao (read)))
            (cond ((equal opcao '0) (funcall voltar))
                  ((not (numberp opcao)) (progn (format t "Escolha uma opção válida~%")))
                  (T
                    (let ((lista (ler-tabuleiros)))
                        (if (or (< opcao 0) (> opcao (length lista)))
                            (progn 
                                (format t "Escolha uma opção válida!") (opcao-tabuleiro 'tabuleiros-menu)
                            )
                            opcao
                        )
                    )
                  )
            )
        )
    )
)

;; FUNCAO INACABADA
;; <resultado>::= (<id-tabuleiro> <algoritmo> <objetivo> <hora-inicio> <solucao> <hora-fim> <pontuacao> <profundidade>)

;; <no>::= (<tabuleiro> <pai> <pontos-objetivo> <pontos-atual> <profundidade> <>)
;; <solucao>::= (<caminho-solucao> <n-abertos> <n-fechados>)
;; <solucao-a*>::= (<caminho-solucao> <n-abertos> <n-fechados> <n-nos-expandidos>)

;; (bfs-recursivo (third (escolher-problema 1)) (second (escolher-problema 1)) 'usar-operadores 'calcular-pontos 'posicao-cavalo 'tabuleiros-cavalo-inicial)

(defun opcao-algoritmo ()
"Recebe a opcao de algoritmo do utilizador e executa-o"
    (progn
        (algoritmos-menu)
        (let ((opcao (read)))
            (cond ((equal opcao '0) (iniciar))
                    ((or (< opcao 0) (> opcao 4)) (progn (format t "Escolha uma opção válida!~%") (opcao-algoritmo)))
                    ((not (numberp opcao)) (progn (format t "Escolha uma opção válida!~%")))
                    (T (let* (
                                (id-tabuleiro (opcao-tabuleiro 'opcao-algoritmo))
                                (problema (escolher-problema id-tabuleiro))
                                (nome (nome-problema problema))
                                (tabuleiro (tabuleiro-problema problema))
                                (objetivo (pontuacao-problema problema))
                                (hora-inicio (hora-atual))
                                (hora-fim (hora-atual))
                            )
                        (ecase opcao
                            (1
                                (let* (
                                    (solucao (bfs-iterativo tabuleiro objetivo 'usar-operadores 'calcular-pontos 'posicionar-cavalo))
                                    (caminho-solucao (no-tabuleiro (car solucao)))
                                    (pontuacao (no-pontos-atual (caar solucao)))
                                    (profundidade (no-profundidade (caar solucao)))
                                    (resultado (list nome 'BFS-iterativo objetivo hora-inicio caminho-solucao hora-fim pontuacao profundidade)))
                                    (progn 
                                        ;;(ficheiro-estatisticas resultado) 
                                        resultado
                                    )
                                )
                            )
                            (2
                                (let (                                    
                                    (solucao (bfs-recursivo tabuleiro objetivo 'usar-operadores 'calcular-pontos 'posicionar-cavalo))
                                    (caminho-solucao (no-tabuleiro (car solucao)))
                                    (pontuacao (no-pontos-atual (caar solucao)))
                                    (profundidade (no-profundidade (caar solucao)))
                                    (resultado (list nome 'BFS-iterativo objetivo hora-inicio caminho-solucao hora-fim pontuacao profundidade)))
                                    (progn 
                                        ;;(ficheiro-estatisticas resultado) 
                                        resultado
                                    )
                                )
                            )
                            #|
                            (3
                                (let* (
                                        (profundidade (opcao-profundidade))
                                        (resultado (list id-tabuleiro 'DFS objetivo (hora-atual) (dfs 'gerar-sucessores profundidade no) (hora-atual) profundidade))
                                    )
                                    (progn
                                        (ficheiro-estatisticas resultado)
                                        resultado
                                    )
                                )
                            )
                            |#
                            #|
                            (4
                                (let* (
                                        (heuristica (opcao-heuristica))
                                        (resultado (list id-tabuleiro 'A* objetivo (hora-atual) (a* 'expandir-no-a* heuristica no) (hora-atual)))
                                    )
                                    (progn
                                        (ficheiro-estatisticas resultado)
                                        resultado
                                    )
                                )
                            )
                            |#
                        )
                    ))
            )
        )
    )
)


#|
(defun opcao-profundidade ()
"Recebe um valor de profundidade máxima do utilizador"
    (if (not (profundidade-menu))
        (let ((opcao (read)))
            (cond ((equal opcao '0) (opcao-objetivo))
                  ((or (not (numberp opcao)) (< opcao 0))
                    (progn
                        (format t "Escolha uma opção válida!~%")
                        (opcao-profundidade 'profundidade-menu)
                    )
                  )
                  (T opcao)
            )
        )
    )
)
|#

#|
(defun opcao-heuristica ()
"Recebe um valor que corresponde a heurística escolhida pelo utilizador"
    (if (not (heuristica-menu))
        (let ((opcao (read)))
            (cond ((equal opcao '0) (opcao-objetivo))
                  ((or (not (numberp opcao)) (< opcao 0) (> opcao 2))
                    (progn
                        (format t "Escolha uma opção válida!~%")
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
|#


;; ============= INPUT/OUTPUT =============

;; (ler-tabuleiros)
(defun ler-tabuleiros ()
"Lê os tabuleiros no ficheiro problemas.dat"
    (with-open-file (stream "problemas.dat" :if-does-not-exist nil)
        (do ((result nil (cons next result))
                (next (read stream nil 'eof) (read stream nil 'eof)))
                    ((equal next 'eof) (reverse result))
        )
    )
)

(defun escolher-problema (indice &optional (lista (ler-tabuleiros)))
"Devolve o problema escolhido através do índice."
    (nth (1- indice) lista)
)

(defun tabuleiro-problema (problema)
    (third problema)
)

(defun pontuacao-problema (problema)
    (second problema)
)

(defun nome-problema (problema)
    (string-upcase (first problema))
)

;; (print-tabuleiro (ler-tabuleiros))
(defun print-tabuleiro (indice &optional (stream t))
"Imprime um tabuleiro do ficheiro problemas.dat, indice comeca no 1"
    (if (<= indice 0) nil      
        (let ((tabuleiroEscolhido (escolher-problema indice)))
            (progn
                (format stream "Problema ~a" (nome-problema tabuleiroEscolhido))
                (format t "~%")
                (format stream "Pontos necessários: ~d" (pontuacao-problema tabuleiroEscolhido))
                (format t "~%")
                (format-tabuleiro (tabuleiro-problema tabuleiroEscolhido))
            )
        )  
    )
)

;; (print-tabuleiros (ler-tabuleiros))
(defun print-tabuleiros (&optional (ln 1))
    (cond
        ((> ln (length (ler-tabuleiros))) (format T "------------------------------------~%") )
        (t 
            (progn
                (format T "~%" )
                (print-tabuleiro ln)
                (print-tabuleiros (1+ ln))
            )
        )
    )
)

;; ============= ESTATISTICAS =============

;; <solucao>::= (<id-tabuleiro> <algoritmo> <objetivo> <hora-inicio> <caminho-solucao> <profundidade> <hora-fim>)
#|
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
|#
(defun hora-atual ()
"Retorna a hora atual (hh mm ss)"
    (multiple-value-bind (s m h)
            (get-decoded-time)
        (format nil "~a:~a:~a" h m s))
)
#|
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
|#