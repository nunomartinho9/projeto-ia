;;  Carrega os outros ficheiros de código, 
;;  escreve e lê ficheiros, e trata da interação com o utilizador
;;  Autores: Nuno Martinho & João Coelho.

;; ============ CARREGAR FICHEIROS ============

(load "procura.lisp")
(load "puzzle.lisp")

;; ============= INICIAR =============

(defun iniciar ()
"Inicializa o programa."
    (menu)
    (susbtituir-f-no-ficheiro)
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
                (progn (format t "Obrigado por jogar!~%~%") (quit)
            ))

            (otherwise (progn (format t "Escolha uma opção válida!") (iniciar)))    
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
"Mostra os tabuleiros disponíveis no programa."
    (cond ((null problemas) 
            (progn
                (format t "~%|                                 |")
                (format t "~%|        0 - Voltar atrás         |") 
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
                    (format t "~%|    ~a - Tabuleiro ~a (aleatório)  |" i (code-char (+ i 64)))
                    (format t "~%|    ~a - Tabuleiro ~a              |" i (code-char (+ i 64)))
                )
                (tabuleiros-menu (+ i 1) (cdr problemas))
            )
        )
    )
)

(defun algoritmos-menu ()
"Mostra os algoritmos disponíveis no programa."
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
        (format t "~%|      - Defina a profundidade máxima         |")
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
"Mostra uma mensagem para escolher a heurística."
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
"Recebe a opção de um tabuleiro do menu."
    (progn 
        (tabuleiros-menu)
        (let ((opcao (read)))
            (cond ((equal opcao 0) (funcall voltar))
                  ((not (numberp opcao)) (progn (format t "Escolha uma opção válida!~%")))
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

;; <resultado>::= (<id-tabuleiro> <algoritmo> <objetivo> <hora-inicio> <solucao> <hora-fim> <profundidade-max>)

;; <no>::= (<tabuleiro> <pai> <pontos-objetivo> <pontos-atual> <profundidade> <>)
;; <solucao>::= (<caminho-solucao> <n-abertos> <n-fechados>)
;; <solucao-a*>::= (<caminho-solucao> <n-abertos> <n-fechados> <n-nos-expandidos>)

(defun opcao-algoritmo ()
"Recebe a opção de algoritmo do utilizador e executa-o."
    (progn
        (algoritmos-menu)
        (let ((opcao (read)))
            (cond ((equal opcao '0) (iniciar))
                    ((or (< opcao 0) (> opcao 4)) (progn (format t "Escolha uma opção válida!~%") (opcao-algoritmo)))
                    ((not (numberp opcao)) (progn (format t "Escolha uma opção válida!~%")))
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
                                    (resultado (list nome 'BFS objetivo (get-universal-time) (bfs-recursivo tabuleiro objetivo 'usar-operadores 'calcular-pontos 'posicao-cavalo 'tabuleiros-cavalo-inicial) (get-universal-time)))
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
                                        (resultado (list nome 'DFS objetivo (get-universal-time) (dfs-recursivo tabuleiro objetivo 'usar-operadores 'calcular-pontos 'posicao-cavalo 'tabuleiros-cavalo-inicial profundidade-max) (get-universal-time) profundidade-max))
                                    )
                                    (progn
                                        (format-solucao resultado)
                                        (if (fifth resultado)
                                            (ficheiro-estatisticas resultado)
                                        )
                                    )
                                )
                            )
                            #|
                            (3
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

(defun opcao-profundidade ()
"Recebe um valor de profundidade máxima do utilizador."
    (if (not (profundidade-menu))
        (let ((opcao (read)))
            (cond ((equal opcao 0) (opcao-tabuleiro))
                  ((equal opcao 1) 20)
                  ((or (not (numberp opcao)) (< opcao 0))
                    (progn
                        (format t "Escolha uma opção válida!~%")
                        (opcao-profundidade 'profundidade-menu)
                    )
                  )
                  (t opcao)
            )
        )
    )
)

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

;; ============ FORMATAR SOLUCAO ============
;; <resultado>::= (<id-tabuleiro> <algoritmo> <objetivo> <hora-inicio> <solucao> <hora-fim>)

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
"Formata o nó-solução."
  (not (null (mapcar #'(lambda (l)
                         (progn
                          (format stream "~%Pontos: ~a~tProfundidade: ~a~%"(solucao-no-pontos-atual l) (solucao-no-profundidade l))
                          (format stream "------------------------------------------------~%")
                          (format-no-solucao l stream)
                          )) nos)))
  (format t "~%")
)

(defun format-solucao (resultado &optional (stream t))
"Formatar a solução completa."
  (let* (
        (problema-id (first resultado))
        (algoritmo (second resultado))
        (objetivo (third resultado))
        (solucao (fifth resultado))
        (profundidade-max (seventh resultado))
    )
    (format stream "====================================================~%")
    (format stream "~tProblema ~a~t~tObjetivo: ~a pontos~%" problema-id objetivo)
    (format stream "~tAlgoritmo ~a~%" algoritmo)
    (if profundidade-max
        (format stream "~tProfundidade máxima: ~a~%" profundidade-max)
    )
    (if solucao
        (progn
            (format stream "~%~tUma solução foi encontrada!~%")
            (format stream "===================================================~%")
            (format-nos-solucao (reverse (solucao-nos solucao)))
            (format stream "===========================================================~%")
            (format stream "Restantes estatísticas guardadas no ficheiro resultados.dat~%")
            (format stream "===========================================================~%")
        )
        (progn
            (format stream "~%~tNão foi encontrada uma solução!~%")
            (format stream "========================================================~%"))
    )
  )
)

;;; ============= INPUT/OUTPUT =============

;; (ler-tabuleiros)
(defun ler-tabuleiros ()
"Lê os tabuleiros no ficheiro problemas.dat."
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
"Devolve o tabuleiro do problema."
    (third problema)
)

(defun pontuacao-problema (problema)
"Devolve a pontuação/objetivo do problema."
    (second problema)
)

(defun nome-problema (problema)
"Devolve a letra do problema."
    (string-upcase (first problema))
)

;; (print-tabuleiro (ler-tabuleiros))
(defun print-tabuleiro (indice &optional (stream t))
"Imprime um tabuleiro do ficheiro problemas.dat (índice começa no 1)."
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

(defun escrever-no-ficheiro (dados caminho-ficheiro)
"Escrever no ficheiro passado como parâmetro."
    (with-open-file (stream caminho-ficheiro :direction :output :if-exists :supersede)
        (dolist (item dados)
            (write item :stream stream)
            (terpri stream)
        )
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

(defun susbtituir-f-no-ficheiro ()
"Susbstitui o tabuleiro do problema F no ficheiro."
    (let* ((dados-ficheiro (ler-tabuleiros))
            (novo-tabuleiro (tabuleiro-aleatorio))
        )
        (when dados-ficheiro
            (let ((dados-atualizados (substituir-tabuleiro-f dados-ficheiro novo-tabuleiro)))
                (if dados-atualizados
                    (escrever-no-ficheiro dados-atualizados "problemas.dat")
                    (format t "Tabuleiro F não gerado!")
                )
            )
        )
    )
)

#|
(defun substituir-valor-lista (lista indice novo-valor)
"Susbtitui um valor numa lista."
    (if (and (listp lista) (>= (length lista) (1+ indice)))
        (let ((antes (subseq lista 0 indice))
                (depois (subseq lista (1+ indice)))
            )
            (append antes (list novo-valor) depois)
        )
      (format t "O índice do elemento não consta na lista."))
)

(defun substituir-lista-ordenada (dados indice-lista nova-lista)
    (if (and (listp dados) (>= (length dados) (1+ indice-lista)))
        (let ((antes (subseq dados 0 indice-lista))
                (depois (subseq dados (1+ indice-lista))))
            (append antes (list nova-lista) depois))
        (format t "O índice da lista está fora dos valores esperados.")
    )
)
|#


;; ============= ESTATISTICAS =============

;;(ficheiro-estatisticas '(<id-tabuleiro> <algoritmo> <objetivo> <hora-inicio> <solucao> <hora-fim> <profundidade-max>))
(defun ficheiro-estatisticas (resultado)
"Ficheiro de resultados estatísticos (solução + dados estatísticos sobre a eficiência)."
    (with-open-file (stream "resultados.dat" :direction :output :if-does-not-exist :create :if-exists :append)
        (estatisticas stream resultado)
    )
)

(defun duracao (hora-inicio hora-fim)
"Calcula a diferença entre dois valores temporais."
    (- hora-fim hora-inicio)
)

(defun estatisticas (stream resultado)
"Solução e dados de eficiência para os algoritmos."
    (let* (
            (problema-id (first resultado))
            (algoritmo (second resultado))
            (objetivo (third resultado))
            (hora-inicio (fourth resultado))
            (solucao (fifth resultado))
            (hora-fim (sixth resultado))
            (profundidade (seventh resultado))
           )
    (progn
        (format stream "~%Tabuleiro ~a" problema-id)
        (format stream "~% - Algoritmo: ~a" algoritmo)
        (format stream "~% - Objetivo: ~a pontos" objetivo)
        (if (eql algoritmo 'DFS)
            (format stream "~% - Profundidade máxima: ~a" profundidade)
        )
        (if solucao 
            (progn
                (format stream "~% - Solução encontrada ~%")
                (format-nos-solucao (reverse (solucao-nos solucao)) stream)
                (format stream "~% - Pontos: ~a" (solucao-no-pontos-atual (no-caminho-solucao-primeiro solucao)))
                (format stream "~% - Profundidade: ~a" (solucao-no-profundidade (no-caminho-solucao-primeiro solucao)))
                (format stream "~% - Nº nós gerados: ~a" (num-nos-gerados solucao))
                (if (eql algoritmo 'A*)
                    (format stream "~% - Nº nós expandidos: ~a" (num-nos-expandidos-a* solucao))
                    (format stream "~% - Nº nós expandidos: ~a" (solucao-fechados solucao))
                )
                (format stream "~% - Penetrância: ~f" (penetrancia solucao))
                (format stream "~% - Fator de ramificação média: ~f" (fator-ramificacao-media solucao))
                (format stream "~% - Duração: ~a segundo(s)" (duracao hora-inicio hora-fim))
                (format stream "~%~%~%")
            )
        )
    ))
)


;;(iniciar)