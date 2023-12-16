;; Código relacionado com o problema
;; Outra para implementar tudo o que envolve a resolução do 
;; problema concreto, incluindo a definição
;; dos operadores e heurísticas, específicos do domínio de aplicação;

;;;; Autor: Nuno Martinho e João Coelho

;; ============= TABULEIROS =============

(defun tabuleiro-teste ()
  "Tabuleiro de teste sem nenhuma jogada realizada"
  '((94 25 54 89 21 8 36 14 41 96)
    (78 47 56 23 5 49 13 12 26 60)
    (0 27 17 83 34 93 74 52 45 80)
    (NIL 9 77 95 55 39 91 73 57 30)
    (24 15 22 86 1 11 68 79 76 72)
    (81 48 32 2 64 16 50 37 29 71)
    (99 51 6 18 53 28 7 63 10 88)
    (59 42 46 85 90 75 87 43 20 31)
    (3 61 58 44 65 82 19 4 35 62)
    (33 70 84 40 66 38 92 67 98 97)))

(defun tabuleiro-jogado ()
  "Tabuleiro de teste igual ao anterior mas tendo sido colocado o cavalo na posi��o: i=0 e j=0"
  '((NIL 25 54 89 T 8 36 14 41 96)
    (78 47 56 23 5 49 13 12 26 60)
    (0 27 17 83 34 93 74 52 45 80)
    (69 9 77 95 55 39 91 73 57 30)
    (24 15 22 86 1 11 68 79 76 72)
    (81 48 32 2 64 16 50 37 29 71)
    (99 51 6 18 53 28 7 63 10 88)
    (59 42 46 85 90 75 87 43 20 31)
    (3 61 58 44 65 82 19 4 35 62)
    (33 70 84 40 66 38 92 67 98 97)))

;; ============= FUNÇÕES AUXILIARES =============

;;(remover-se #'(lambda (x) (= x 0)) '(1 2 0 2 0 4)) -> (1 2 2 4)
(defun remover-se (pred lista)
  "Reconstrói uma lista sem os elementos que verificam o predicado passado como argumento."
  (cond ((null lista) NIL)
        ((funcall pred (car lista)) (remover-se pred (cdr lista)))
        (T (cons (car lista) (remover-se pred (cdr lista))))))

;; (soma-tabuleiro (tabuleiro-teste))
(defun somar-tabuleiro (tabuleiro)
  "Soma todos os valores do tabuleiro."
  (reduce #'+ (mapcan #'(lambda (linha) (remove-if #'(lambda (celula) (eq celula nil)) linha)) tabuleiro)))

;; (linha 0 (tabuleiro-teste))
;; (94 25 54 89 21 8 36 14 41 96)
(defun linha (index tabuleiro)
  "Função que recebe um índice e o tabuleiro e retorna uma lista que representa essa linha do 
tabuleiro"
  (nth index tabuleiro))

;;  (celula 0 1 (tabuleiro-teste))
;; 25
(defun celula (lin col tabuleiro)
  "Função que recebe dois índices e o tabuleiro e retorna o valor presente nessa célula do
tabuleiro"
  (if (or (< lin 0) (< col 0)) NIL (linha col (linha lin tabuleiro))))

;; (lista-numeros)
#|
  (99 98 97 96 95 94 93 92 91 90 89 88 87 86 85 84 83 82 81 80 79 78 77 76 75 74
 73 72 71 70 69 68 67 66 65 64 63 62 61 60 59 58 57 56 55 54 53 52 51 50 49 48
 47 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22
 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0)
|#
(defun lista-numeros (&optional (n 100))
  "Função que recebe um número positivo n e cria uma lista com todos os números
entre 0 (inclusivé) e o número passado como argumento (exclusivé). Por default o n é 100."
  (cond
   ((= n 1) (cons '0 '()))
   (t (cons (1- n) (lista-numeros (1- n))))))


;; (baralhar (lista-numeros))
#|
  (7 55 2 94 39 54 61 26 72 82 0 63 80 40 34 49 3 71 27 85 37 42 91 41 44 89 19
 78 99 4 46 6 24 21 83 70 5 36 60 96 18 17 95 35 33 81 56 79 51 31 86 9 97 76
 57 12 14 90 67 53 73 62 74 65 28 75 22 45 64 30 20 52 69 32 84 15 25 1 93 8 58
 50 87 77 59 68 66 16 29 92 23 88 47 13 11 38 43 10 98 48)
|#
(defun baralhar (lista)
  "Função que recebe uma lista e irá mudar aleatoriamente os seus números"
  (cond
   ((null lista) lista)
   (t
     (let ((num (nth (random (length lista)) lista)))
       (cons num (baralhar (remover-se #'(lambda (x) (= num x)) lista)))))))

;; (tabuleiro-aleatorio)
(defun tabuleiro-aleatorio (&optional (lista (baralhar (lista-numeros))) (n 10))
  "Função que gera um tabuleiro n x n (10 x 10 valor default)"
  (cond
   ((null lista) nil)
   (t (cons (subseq lista 0 n) (tabuleiro-aleatorio (subseq lista n) n)))))

;; (substituir-posicao 0 (linha 0 (tabuleiro-teste)))
;; (NIL 25 54 89 21 8 36 14 41 96)
;; (substituir-posicao 0 (linha 0 (tabuleiro-teste)) T)
;; (T 25 54 89 21 8 36 14 41 96)
(defun substituir-posicao (index lista &optional (val NIL))
  "Função que recebe um índice, uma lista e um valor (por default o valor é NIL) e
substitui pelo valor pretendido nessa posição"
  (cond
   ((null lista) '())
   ((= index 0) (cons val (cdr lista)))
   (t (cons (car lista) (substituir-posicao (1- index) (cdr lista) val)))))

;;  (substituir 0 0 (tabuleiro-teste) T)
(defun substituir (lin col tabuleiro &optional (val NIL))
  "Função que recebe dois índices, o tabuleiro e um valor (por default o valor é NIL). A
função deverá retornar o tabuleiro com a célula substituída pelo valor pretendido"
  (cond
   ((null tabuleiro) '())
   ((or (eq nil lin) (eq nil col)) tabuleiro);; ver isto depois
   ((= lin 0) (cons (substituir-posicao col (linha lin tabuleiro) val) (cdr tabuleiro)))
   (t (cons (car tabuleiro) (substituir (1- lin) col (cdr tabuleiro) val)))))

;; (posicao-valor 15 (tabuleiro-teste))
;; (4 1)
;; melhorar isto deixar em vez do 9 trocar por n x n
(defun posicao-valor (valor tabuleiro &optional (row 0) (column 0))
  "Função que recebe o tabuleiro e devolve a posição (i j) em que se encontra o (valor)."
  (cond
   ((null tabuleiro) nil)
   ((eq valor (celula row column tabuleiro)) (list row column))
   ((< 9 row) nil)
   ((< 9 column) (posicao-valor valor tabuleiro (1+ row)))
   (t (posicao-valor valor tabuleiro row (1+ column)))))

;; (posicao-cavalo (tabuleiro-teste))
;; NIL
;; (posicao-cavalo (tabuleiro-jogado))
;; (0 0)

(defun posicao-cavalo (tabuleiro)
  "Função que recebe o tabuleiro e devolve a posição (i j) em que se encontra o cavalo."
  (posicao-valor T tabuleiro))


;;(contar-casas-validas (linha 0 (tabuleiro-teste)))
;;10
(defun contar-casas-validas (linha)
  "Função que conta casas válidas em que o jogador pode jogar"
  (cond
   ((null linha) 0)
   ((eq (car linha) nil) (contar-casas-validas (cdr linha)))
   (t (1+ (contar-casas-validas (cdr linha))))))

(defun casas-validas-posicoes (tabuleiro &optional (casas (linha 0 tabuleiro)))

  (mapcar #'(lambda (l)
              (posicao-valor l tabuleiro))
    (remover-se #'(lambda (x) (eq x nil)) casas)))


;;(format-tabuleiro (posicionar-cavalo (tabuleiro-teste)))
(defun posicionar-cavalo (tabuleiro)
  "Posicionar o cavalo (T) na primeira linha e numa coluna aleatoria válida
    (devolve o tabuleiro com o cavalo posicionado.)"
  (if (eq nil (posicao-cavalo tabuleiro))

      ;;meter cavalo numa posicao aleatoria na linha 0 (primeira linha)
      #|(substituir 0 (random
                      (contar-casas-validas (linha 0 tabuleiro))) tabuleiro T)|#
      (let* ((tabuleiroJogada (substituir 0 (cadr (nth (random (contar-casas-validas (linha 0 tabuleiro))) (casas-validas-posicoes tabuleiro))) tabuleiro T))
             (duplo (nth (random (length (posicao-duplos tabuleiroJogada))) (posicao-duplos tabuleiroJogada)))
             (pos (posicao-cavalo tabuleiroJogada)))
        (if (eq t (duplop (celula (car pos) (cadr pos) tabuleiro)))
            (substituir (car duplo) (cadr duplo) tabuleiroJogada)
            (eliminar-simetrico (celula (car pos) (cadr pos) tabuleiro) tabuleiroJogada)))

      ;;ja existe cavalo por isso nao fazer nada (devolver tabuleiro).
      tabuleiro))



(defun posicionar-cavalo2 (tabuleiro &optional(lin 0) (col 0))
  (substituir lin col tabuleiro T)
  
  )

(defun tabuleiros-cavalo-inicial (tabuleiro-sem-cavalo)
  (mapcar #'(lambda (pos)
              (posicionar-cavalo2 tabuleiro-sem-cavalo (car pos) (cadr pos))
              ) 
      (casas-validas-posicoes tabuleiro-sem-cavalo))
  
  )

;; (n-simetrico 96)
;; 69
(defun n-simetrico (num &optional (sim 0))
  "Transforma o número passado por argumento (num) para o seu simetrico."
  (cond
   ((= num 0) sim)
   (t
     (n-simetrico (/ (- num (rem num 10)) 10) (+ (* sim 10) (rem num 10))))))

;; (posicao-simetrico 96 (tabuleiro-teste))
;;(3 0)
(defun posicao-simetrico (num tabuleiro)
  "Função que recebe o tabuleiro e um numero e devolve a posição (i j) em 
    que se encontra o numero simetrico de (num)"
  (posicao-valor (n-simetrico num) tabuleiro))

;; (eliminar-simetrico 96 (tabuleiro-teste))
#|
  ((94 25 54 89 21 8 36 14 41 96) (78 47 56 23 5 49 13 12 26 60)
 (0 27 17 83 34 93 74 52 45 80) (NIL 9 77 95 55 39 91 73 57 30)
 (24 15 22 86 1 11 68 79 76 72) (81 48 32 2 64 16 50 37 29 71)
 (99 51 6 18 53 28 7 63 10 88) (59 42 46 85 90 75 87 43 20 31)
 (3 61 58 44 65 82 19 4 35 62) (33 70 84 40 66 38 92 67 98 97))
|#
(defun eliminar-simetrico (num tabuleiro)
  "Função que elimina o simetrico de (num) no tabuleiro"
  (let ((coordenadas-sim (posicao-simetrico num tabuleiro)))
    (substituir (car coordenadas-sim) (cadr coordenadas-sim) tabuleiro)))

;; (duplop 22)
;; T
;; (duplop 15)
;; NIL
(defun duplop (num &optional (max 99))
  "Função que verifica se o numero passado como argumento é duplo"
  (cond
   ((> num max) nil)
   ((= 0 (rem num 11)) t)
   (t nil)))

;; lista com todos os duplos ate 99
(defun lista-duplos ()
  (list 11 22 33 44 55 66 77 88 99))

;; (posicao-duplos (tabuleiro-teste))
;; ((4 5) (4 2) (9 0) (8 3) (3 4) (9 4) (3 2) (6 9) (6 0))
(defun posicao-duplos (tabuleiro)
  "Função que devolve uma lista com as posicoes dos numeros duplos"
  (remover-se #'(lambda (x) (eq x nil))
              (mapcar #'(lambda (dp)
                          (posicao-valor dp tabuleiro)) (lista-duplos))))


;; calcular score de um no
(defun calcular-pontos (pontos-atual tabuleiro-anterior tabuleiro-novo)
  "Recebe a pontuação atual e com o tabuleiro anterior e o novo tabuleiro é devolvido a nova pontuação."
  (let* ((pos (posicao-cavalo tabuleiro-novo))
         (pontos (celula (car pos) (cadr pos) tabuleiro-anterior)))
    (+ pontos-atual pontos)))

;; ============= OPERADORES =============

(defun mover-cavalo (tabuleiro &optional (valLinha 0) (valColuna 0))
  "Função auxiliar para os operadores."
  (let* ((tabuleiroComCavalo tabuleiro)
         (lin (car (posicao-cavalo tabuleiroComCavalo)))
         (col (cadr (posicao-cavalo tabuleiroComCavalo)))
         (novaPosicaoCavalo (celula (+ lin valLinha) (+ col valColuna) tabuleiroComCavalo)))

    (cond
     ((eq novaPosicaoCavalo NIL) NIL) ;;posicao invalida, movimento ilegal.

     (t
       (let* ((tabuleiroJogada (substituir (+ lin valLinha) (+ col valColuna) (substituir lin col tabuleiroComCavalo) T))
              (duplos-list (posicao-duplos tabuleiroJogada))
              (duplo (if duplos-list
                         (nth (random (length duplos-list)) duplos-list)
                         nil)))
         (if (eq t (duplop novaPosicaoCavalo))
             (substituir (car duplo) (cadr duplo) tabuleiroJogada)
             (eliminar-simetrico novaPosicaoCavalo tabuleiroJogada)))))))


(defun operador-1 (tabuleiro)
  "mover o cavalo 2 linhas para baixo e uma coluna para a esquerda"
  (mover-cavalo tabuleiro 2 -1))


(defun operador-2 (tabuleiro)
  "mover o cavalo 2 linhas para baixo e uma coluna para a direita"
  (mover-cavalo tabuleiro 2 1))


(defun operador-3 (tabuleiro)
  "mover o cavalo 1 linha para baixo e duas colunas para a direita"
  (mover-cavalo tabuleiro 1 2))


(defun operador-4 (tabuleiro)
  "mover o cavalo 1 linha para cima e duas colunas para a direita"
  (mover-cavalo tabuleiro -1 2))

(defun operador-5 (tabuleiro)
  "mover o cavalo 2 linhas para cima e 1 coluna para a direita"
  (mover-cavalo tabuleiro -2 1))

(defun operador-6 (tabuleiro)
  "mover o cavalo 2 linhas para cima e 1 coluna para a esquerda"
  (mover-cavalo tabuleiro -2 -1))

(defun operador-7 (tabuleiro)
  "mover o cavalo 1 linha para cima e 2 colunas para a esquerda"
  (mover-cavalo tabuleiro -1 -2))


(defun operador-8 (tabuleiro)
  "mover o cavalo 1 linha para baixo e 2 colunas para a esquerda"
  (mover-cavalo tabuleiro 1 -2))


;; (format-tabuleiros (usar-operadores (tabuleiro-teste)))
;; (format-tabuleiros (usar-operadores (tabuleiro-jogado)))
(defun usar-operadores (tabuleiro)
  "Lista de tabuleiros com cada jogada possivel."
  (remover-se #'(lambda (x) (eq x nil))
              (list
               (operador-1 tabuleiro)
               (operador-2 tabuleiro)
               (operador-3 tabuleiro)
               (operador-4 tabuleiro)
               (operador-5 tabuleiro)
               (operador-6 tabuleiro)
               (operador-7 tabuleiro)
               (operador-8 tabuleiro))))

;; (format-tabuleiro (tabuleiro-teste))
(defun format-tabuleiro (tabuleiro &optional (stream t))
  "Formata o tabuleiro"
  (not (null (mapcar #'(lambda (l)
                         (progn
                          (mapcar #'(lambda (e)
                                      (format stream "~5a" e)) l)
                          (format stream "~%"))) tabuleiro)))
  (format t "~%"))

;;(format-tabuleiros (usar-operadores (tabuleiro-jogado)))
;;(format-tabuleiros (usar-operadores (tabuleiro-teste)))
(defun format-tabuleiros (tabuleiros &optional (stream t))
  "Formata os tabuleiros"
  (not (null (mapcar #'(lambda (l)
                         (progn
                          (format-tabuleiro l)
                          (format stream "------------------------------------------------~%"))) tabuleiros)))
  (format t "~%~%"))
