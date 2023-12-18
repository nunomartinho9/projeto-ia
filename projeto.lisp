;;  Carrega os outros ficheiros de codigo, 
;;  escreve e le ficheiros, e trata da interacao com o utilizador
;;  Autores: Nuno Martinho e Joao Coelho.

;; ============ CARREGAR FICHEIROS ============

(defun diretorio ()
    "C:/Users/joaor/OneDrive/Documentos/IPS/ESTS/LEI/3_ANO/IA/Projeto/ia-projeto1/"
)

(compile-file (concatenate 'string (diretorio) "procura.lisp"))
(compile-file (concatenate 'string (diretorio) "puzzle.lisp"))
(load (concatenate 'string (diretorio) "puzzle.lisp"))
(load (concatenate 'string (diretorio) "procura.lisp"))

;; ============= INICIAR =============

(defun jogar ()
"Inicializa o jogo."
    (progn 
        (substituir-f-no-ficheiro)
        (iniciar)
    )
)

(defun iniciar ()
"Inicializa o programa."
    (menu)
    (let ((opcao (read)))
        (case opcao
            (1 
                (progn 
                    (print-tabuleiros)
                    (iniciar)
            ))
            (2
                (progn
                    (opcao-algoritmo)
                    (iniciar)
            ))
            (3 
                (progn
                    (finish-output)
                    (clear-output)
                    (format t "Obrigado por jogar!~%~%")
            ))

            (otherwise (progn (format t "Escolha uma opcao valida!") (iniciar)))    
        )
    )
)

;; ============= MENUS =============

(defun menu ()
  "Mostra o menu inicial."
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
"Mostra os tabuleiros disponiveis no programa."
    (cond ((null problemas) 
            (progn
                (format t "~%|                                 |")
                (format t "~%|        0 - Voltar atras         |") 
                (format t "~%o                                 o")
                (format t "~%~%>> ")
            )
        )
        (T (progn
                (if (= i 1) 
                    (progn 
                        (format t "~%o                                 o")
                        (format t "~%|    - Escolha o tabuleiro: -     |")
                        (format t "~%|                                 |")
                    )
                )
                (if (eq i 6)
                    (format t "~%|    ~a - Tabuleiro ~a (aleatorio)  |" i (code-char (+ i 64)))
                    (format t "~%|    ~a - Tabuleiro ~a              |" i (code-char (+ i 64)))
                )
                (tabuleiros-menu (+ i 1) (cdr problemas))
            )
        )
    )
)

(defun algoritmos-menu ()
"Mostra os algoritmos disponiveis no programa."
    (progn
        (format t "~%o                                    o")
        (format t "~%|      - Escolha o algoritmo -       |")
        (format t "~%|                                    |")
        (format t "~%|         1 - Breadth-First          |")
        (format t "~%|         2 - Depth-First            |")
        (format t "~%|         3 - A*                     |")
        (format t "~%|                                    |")
        (format t "~%|            0 - Voltar              |")
        (format t "~%o                                    o")
        (format t "~%~%>> ")
    )
)

(defun profundidade-menu ()
"Mostra uma mensagem para escolher a profundidade."
    (progn
        (format t "~%o                                             o")
        (format t "~%|      - Defina a profundidade maxima         |")
        (format t "~%|                a utilizar -                 |")
        (format t "~%|                                             |")
        (format t "~%|         1 - Valor predefinido (20)          |")
        (format t "~%|                                             |")
        (format t "~%|          Ou introduza outro valor.          |")
        (format t "~%|                                             |")
        (format t "~%|                0 - Voltar                   |")
        (format t "~%o                                             o")
        (format t "~%~%>> ")
    )
)

(defun heuristica-menu ()
"Mostra uma mensagem para escolher a heuristica."
    (progn
        (format t "~%o                                                o")
        (format t "~%|       - Defina a heuristica a utilizar -       |")
        (format t "~%|                                                |")
        (format t "~%|           1 - Heuristica Enunciado             |")
        ;(format t "~%|         2 - Heuristica Personalizada           |")
        (format t "~%|                                                |")
        (format t "~%|                  0 - Voltar                    |")
        (format t "~%o                                                o")
        (format t "~%~%>> ")
    )
)

;; ============= NAVEGACAO =============

(defun opcao-tabuleiro (&optional (voltar 'iniciar))
"Recebe a opcao de um tabuleiro do menu."
    (progn 
        (tabuleiros-menu)
        (let ((opcao (read)))
            (cond ((equal opcao 0) (funcall voltar))
                  ((not (numberp opcao)) (progn (format t "Escolha uma opcao valida!~%")))
                  (T
                    (let ((lista (ler-tabuleiros)))
                        (if (or (< opcao 0) (> opcao (length lista)))
                            (progn 
                                (format t "Escolha uma opcao valida!") (opcao-tabuleiro 'tabuleiros-menu)
                            )
                            opcao     
                        )
                    )
                  )
            )
        )
    )
)

;; <resultado>::= (<id-tabuleiro> <algoritmo> <objetivo> <hora-inicio> <solucao> <hora-fim> <heuristica/profundidade>)

;; <no>::= (<tabuleiro> <pai> <pontos-objetivo> <pontos-atual> <profundidade> <>)
;; <solucao>::= (<caminho-solucao> <n-abertos> <n-fechados>)
;; <solucao-a*>::= (<caminho-solucao> <n-abertos> <n-fechados> <n-nos-expandidos>)

(defun opcao-algoritmo ()
"Recebe a opcao de algoritmo do utilizador e executa-o."
    (progn
        (algoritmos-menu)
        (let ((opcao (read)))
            (cond ((equal opcao '0) (iniciar))
                    ((or (< opcao 0) (> opcao 4)) (progn (format t "Escolha uma opcao valida!~%") (opcao-algoritmo)))
                    ((not (numberp opcao)) (progn (format t "Escolha uma opcao valida!~%")))
                    (T (let* (
                                (id-tabuleiro (opcao-tabuleiro))
                                (problema (escolher-problema id-tabuleiro))
                                (nome (nome-problema problema))
                                (tabuleiro (tabuleiro-problema problema))
                                (objetivo (pontuacao-problema problema))
                            )
                        (ecase opcao
                            (1
                                (let* (                                   
                                    (resultado (list nome 'BFS objetivo (get-internal-real-time) (bfs-recursivo tabuleiro objetivo 'usar-operadores 'calcular-pontos 'posicao-cavalo 'tabuleiros-cavalo-inicial) (get-internal-real-time)))
                                    )
                                    (progn
                                        (format-solucao resultado)
                                        (if (fifth resultado)
                                            (ficheiro-estatisticas resultado)
                                        )
                                    )
                                )
                            )
                            (2
                                (let* (
                                        (profundidade-max (opcao-profundidade))
                                        (resultado (list nome 'DFS objetivo (get-internal-real-time) (dfs-recursivo tabuleiro objetivo 'usar-operadores 'calcular-pontos 'posicao-cavalo 'tabuleiros-cavalo-inicial profundidade-max) (get-internal-real-time) profundidade-max))
                                    )
                                    (progn
                                        (format-solucao resultado)
                                        (if (fifth resultado)
                                            (ficheiro-estatisticas resultado)
                                        )
                                    )
                                )
                            )
                            (3
                                (let* (
                                        (heuristica (opcao-heuristica))
                                        (resultado (list nome 'A* objetivo (get-internal-real-time) (a* tabuleiro objetivo 'usar-operadores 'calcular-pontos 'posicao-cavalo 'tabuleiros-cavalo-inicial heuristica) (get-internal-real-time) heuristica))
                                    )
                                    (progn
                                        (format-solucao resultado)
                                        (if (fifth resultado)
                                            (ficheiro-estatisticas resultado)
                                        )
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
"Recebe um valor de profundidade maxima do utilizador."
    (if (not (profundidade-menu))
        (let ((opcao (read)))
            (cond ((equal opcao 0) (iniciar))
                  ((equal opcao 1) 20)
                  ((or (not (numberp opcao)) (< opcao 0))
                    (progn
                        (format t "Escolha uma opcao valida!~%")
                        (opcao-profundidade)
                    )
                  )
                  (t opcao)
            )
        )
    )
)


(defun opcao-heuristica ()
"Recebe um valor que corresponde a heuristica escolhida pelo utilizador"
    (if (not (heuristica-menu))
        (let ((opcao (read)))
            (cond ((equal opcao '0) (iniciar))
                  ((or (not (numberp opcao)) (< opcao 0) (> opcao 2))
                    (progn
                        (format t "Escolha uma opcao valida!~%")
                        (opcao-heuristica)
                    )
                  )
                  (T (ecase opcao
                        (1
                            'heuristica-base
                        )
#|
                        (2
                            'heuristica-top
                        )
|#
                  ))
            )
        )
    )
)


;; ============ FORMATAR SOLUCAO ============
;; <resultado>::= (<id-tabuleiro> <algoritmo> <objetivo> <hora-inicio> <solucao> <hora-fim> <heuristica/profundidade>)

(defun format-no-solucao (no &optional (stream t))
"Formata o tabuleiro."
  (not (null (mapcar #'(lambda (l)
                         (progn
                          ;;(format stream "Objetivo: ~a~tPontos: ~a~tProfundidade: ~a" (solucao-no-pontos-obj no) (solucao-no-pontos-atual no) (solucao-no-profundidade no))
                          (mapcar #'(lambda (e)
                                      (format stream "~5a" e)) l)
                          (format stream "~%"))) (no-tabuleiro no))))
  (format t "~%")
  )

(defun format-nos-solucao (nos &optional (stream t))
"Formata o no-solucao."
  (not (null (mapcar #'(lambda (l)
                         (progn
                          (format stream "~%Pontos: ~a~tProfundidade: ~a~%"(solucao-no-pontos-atual l) (solucao-no-profundidade l))
                          (format stream "------------------------------------------------~%")
                          (format-no-solucao l stream)
                          )) nos)))
  (format t "~%")
)

(defun format-solucao (resultado &optional (stream t))
"Formatar a solucao completa."
  (let* (
        (problema-id (first resultado))
        (algoritmo (second resultado))
        (objetivo (third resultado))
        (solucao (fifth resultado))
        (opcional (seventh resultado))
    )
    (format stream "====================================================~%")
    (format stream "~tProblema ~a~t~tObjetivo: ~a pontos~%" problema-id objetivo)
    (format stream "~tAlgoritmo ~a~%" algoritmo)
    (if opcional
        (cond ((eq algoritmo 'DFS) (format stream "~tProfundidade maxima: ~a~%" opcional))
              ((eq algoritmo 'A*) (format stream "~tHeuristica: ~a~%" opcional)))
        
    )
    (if solucao
        (progn
            (format stream "~%~tUma solucao foi encontrada!~%")
            (format stream "===================================================~%")
            (format-nos-solucao (reverse (solucao-nos solucao)))
            (format stream "===========================================================~%")
            (format stream "Restantes estatisticas guardadas no ficheiro resultados.dat~%")
            (format stream "===========================================================~%")
        )
        (progn
            (format stream "~%~tNao foi encontrada uma solucao!~%")
            (format stream "========================================================~%"))
    )
  )
)

;;; ============= INPUT/OUTPUT =============

;; (ler-tabuleiros)
(defun ler-tabuleiros ()
"Le os tabuleiros no ficheiro problemas.dat."
    (with-open-file (stream (concatenate 'string (diretorio) "problemas.dat") :if-does-not-exist nil)
        (do ((result nil (cons next result))
                (next (read stream nil 'eof) (read stream nil 'eof)))
                    ((equal next 'eof) (reverse result))
        )
    )
)

(defun escolher-problema (indice &optional (lista (ler-tabuleiros)))
"Devolve o problema escolhido atraves do indice."
    (nth (1- indice) lista)
)

(defun tabuleiro-problema (problema)
"Devolve o tabuleiro do problema."
    (third problema)
)

(defun pontuacao-problema (problema)
"Devolve a pontuacao/objetivo do problema."
    (second problema)
)

(defun nome-problema (problema)
"Devolve a letra do problema."
    (string-upcase (first problema))
)

;; (print-tabuleiro (ler-tabuleiros))
(defun print-tabuleiro (indice &optional (stream t))
"Imprime um tabuleiro do ficheiro problemas.dat (indice comeca no 1)."
    (if (<= indice 0) nil      
        (let ((tabuleiroEscolhido (escolher-problema indice)))
            (progn
                (format stream "Problema ~a" (nome-problema tabuleiroEscolhido))
                (format t "~%")
                (format stream "Pontos necessarios: ~d" (pontuacao-problema tabuleiroEscolhido))
                (format t "~%")
                (format-tabuleiro (tabuleiro-problema tabuleiroEscolhido))
            )
        )  
    )
)

;; (print-tabuleiros (ler-tabuleiros))
(defun print-tabuleiros (&optional (ln 1))
"Imprime todos os tabuleiros no ficheiro problemas.dat."
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

;; ============= TABULEIRO F =============

(defun escrever-no-ficheiro (dados)
"Escrever no ficheiro passado como parametro."
    (with-open-file (stream (concatenate 'string (diretorio) "problemas.dat") :direction :output :if-exists :supersede)
        (dolist (item dados)
            (write item :stream stream)
            (terpri stream)
        )
        (finish-output stream)
        (close stream)
    )
)

(defun substituir-tabuleiro-f (dados novo-tabuleiro)
"Substitui o tabuleiro do problema F."
  (let ((dados-atualizados (mapcar (lambda (item)
                                (if (and (listp item) (string= "f" (car item)))
                                    (list (car item) (cadr item) novo-tabuleiro)
                                    item)
                                ) dados)
        )) dados-atualizados
    )
)

(defun substituir-f-no-ficheiro ()
"Susbstitui o tabuleiro do problema F no ficheiro."
    (let* ((dados-ficheiro (ler-tabuleiros))
            (novo-tabuleiro (tabuleiro-aleatorio))
        )
        (when dados-ficheiro
            (let ((dados-atualizados (substituir-tabuleiro-f dados-ficheiro novo-tabuleiro)))
                (if dados-atualizados
                    (escrever-no-ficheiro dados-atualizados)
                    (format t "Tabuleiro F nao gerado!")
                )
            )
        )
    )
)


;; ============= ESTATISTICAS =============

(defun ficheiro-estatisticas (resultado)
"Ficheiro de resultados estatisticos (solucao + dados estatisticos sobre a eficiencia)."
    (with-open-file (stream (concatenate 'string (diretorio) "resultados.dat") :direction :output :if-does-not-exist :create :if-exists :append)
        (estatisticas stream resultado)
        (finish-output stream)
        (close stream)
    )
)

(defun duracao (hora-inicio hora-fim)
"Calcula a diferenca entre dois valores temporais."
; 1 segundo = 1000 milissegundos = 1000000 microsegundos
    (let* (
           (diferenca (- hora-fim hora-inicio))
           (segundos (float (/ diferenca 1000)))
        )
        (format nil "~ds" segundos)
    )
)

(defun estatisticas (stream resultado)
"Solucao e dados de eficiencia para os algoritmos."
    (let* (
            (problema-id (first resultado))
            (algoritmo (second resultado))
            (objetivo (third resultado))
            (hora-inicio (fourth resultado))
            (solucao (fifth resultado))
            (hora-fim (sixth resultado))
            (opcional (seventh resultado))
            (tempo (duracao hora-inicio hora-fim))
           )
    (progn
        (format stream "~%Tabuleiro ~a" problema-id)
        (format stream "~% - Algoritmo: ~a" algoritmo)
        (format stream "~% - Objetivo: ~a pontos" objetivo)
        (cond ((eql algoritmo 'DFS)
                (format stream "~% - Profundidade maxima: ~a" opcional))
            ((eql algoritmo 'A*) (format stream "~% - Heuristica: ~a" opcional))
        )
        (if solucao 
            (progn
                (format stream "~% - Solucao encontrada ~%")
                (format-nos-solucao (reverse (solucao-nos solucao)) stream)
                (format stream "~% - Pontos: ~a" (solucao-no-pontos-atual (no-caminho-solucao-primeiro solucao)))
                (format stream "~% - Profundidade: ~a" (solucao-no-profundidade (no-caminho-solucao-primeiro solucao)))
                (format stream "~% - Nos gerados: ~a" (num-nos-gerados solucao))
                (if (eql algoritmo 'A*)
                    (format stream "~% - Nos expandidos: ~a" (num-nos-expandidos-a* solucao))
                    (format stream "~% - Nos expandidos: ~a" (solucao-fechados solucao))
                )
                (format stream "~% - Penetrancia: ~f" (penetrancia solucao))
                (format stream "~% - Fator de ramificacao media: ~f" (fator-ramificacao-media solucao))
                (format stream "~% - Duracao: ~a" tempo)
                (format stream "~%~%~%")
            )
        )
    ))
)

