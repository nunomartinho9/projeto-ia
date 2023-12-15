;;  Carrega os outros ficheiros de código, 
;;  escreve e lê ficheiros, e trata da interação com o utilizador
;;  Autores: Nuno Martinho & João Coelho.


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
                    ('1 (progn 
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
                                    (print-tabuleiro (car (fifth solucao)))
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
;; <solucao>::= (<id-tabuleiro> <algoritmo> <objetivo> <hora-inicio> <caminho-solucao> <hora-fim> <profundidade>)
#|
(defun opcao-algoritmo ()
"Recebe a opcao de algoritmo do utilizador e executa-o"
    (progn
        (algoritmos-menu)
        (let ((opcao (read)))
            (cond ((equal opcao '0) (iniciar))
                    ((or (< opcao 0) (> opcao 4)) (progn (format t "Escolha uma opção válida!~%") (opcao-algoritmo)))
                    ((not (numberp opcao)) (progn (format t "Escolha uma opção válida!~%")))
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
|#

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
    (first problema)
)

;; (print-tabuleiro (ler-tabuleiros))
(defun print-tabuleiro (indice &optional (stream t))
"Imprime um tabuleiro do ficheiro problemas.dat, indice comeca no 1"
    (if (<= indice 0) nil      
        (let ((tabuleiroEscolhido (escolher-problema indice)))
            (progn
                (format stream "Problema: ~a" (nome-problema tabuleiroEscolhido))
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
                (format T "~%" )
                (print-tabuleiros (1+ ln))
            )
        )
    )
)