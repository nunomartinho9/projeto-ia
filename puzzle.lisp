;; Código relacionado com o problema
;; Outra para implementar tudo o que envolve a resolução do 
;; problema concreto, incluindo a definição
;; dos operadores e heurísticas, específicos do domínio de aplicação;

;;;; Autor: Nuno Martinho e João Coelho


;;; Tabuleiros


(defun tabuleiro-teste ()
"Tabuleiro de teste sem nenhuma jogada realizada"
  '(
    (94 25 54 89 21 8 36 14 41 96) 
    (78 47 56 23 5 49 13 12 26 60) 
    (0 27 17 83 34 93 74 52 45 80) 
    (69 9 77 95 55 39 91 73 57 30) 
    (24 15 22 86 1 11 68 79 76 72) 
    (81 48 32 2 64 16 50 37 29 71) 
    (99 51 6 18 53 28 7 63 10 88) 
    (59 42 46 85 90 75 87 43 20 31) 
    (3 61 58 44 65 82 19 4 35 62) 
    (33 70 84 40 66 38 92 67 98 97)
    )
)

(defun tabuleiro-jogado ()
"Tabuleiro de teste igual ao anterior mas tendo sido colocado o cavalo na posi��o: i=0 e j=0"
  '(
    (NIL 25 54 89 21 8 36 14 41 96) 
    (78 47 56 23 5 T 13 12 26 60) 
    (0 27 17 83 34 93 74 52 45 80) 
    (69 9 77 95 55 39 91 73 57 30) 
    (24 15 22 86 1 11 68 79 76 72) 
    (81 48 32 2 64 16 50 37 29 71) 
    (99 51 6 18 53 28 7 63 10 88) 
    (59 42 46 85 90 75 87 43 20 31) 
    (3 61 58 44 65 82 19 4 35 62) 
    (33 70 84 40 66 38 92 67 98 97)
    )
)

;;(remover-se #'(lambda (x) (= x 0)) '(1 2 0 2 0 4)) -> (1 2 2 4)
(defun remover-se(pred lista)
"Reconstrói uma lista sem os elementos que verificam o predicado passado como argumento."
  (cond ((null lista) NIL) 
        ((funcall pred (car lista)) (remover-se pred (cdr lista)))
        (T (cons (car lista) (remover-se pred (cdr lista))))))

;; (linha 0 (tabuleiro-teste))
(defun linha (index board)
"Função que recebe um índice e o tabuleiro e retorna uma lista que representa essa linha do 
tabuleiro"
  (nth index board)
)

;;  (celula 0 1 (tabuleiro-teste))
(defun celula (lin col board)
"Função que recebe dois índices e o tabuleiro e retorna o valor presente nessa célula do
tabuleiro"
    (if (or (< lin 0) (< col 0)) NIL (linha col (linha lin board)))
  
)

;; (lista-numeros)
(defun lista-numeros (&optional (n 100))
"Função que recebe um número positivo n e cria uma lista com todos os números
entre 0 (inclusivé) e o número passado como argumento (exclusivé). Por default o n é 100."
  (cond 
    ( (= n 1) (cons '0 '()))
    (t (cons (1- n) (lista-numeros (1- n))))
  )
)


;; (baralhar (lista-numeros))
(defun baralhar (lista)
"Função que recebe uma lista e irá mudar aleatoriamente os seus números"
  (cond 
    ( (null lista) lista)
    (t 
      (let (  (num (nth (random (length lista)) lista)) )
        (cons num (baralhar ( remover-se #'(lambda (x) (= num x)) lista )))
      )
    )
  )
  
)

;; (tabuleiro-aleatorio)
(defun tabuleiro-aleatorio (&optional (lista  (baralhar (lista-numeros)) ) (n 10))
"Função que gera um tabuleiro n x n (10 x 10 valor default)"
 (cond
 ((null lista) nil)
 (t (cons (subseq lista 0 n) (tabuleiro-aleatorio (subseq lista n) n)))
 )
)

;; (substituir-posicao 0 (linha 0 (tabuleiro-teste)))
;; (NIL 25 54 89 21 8 36 14 41 96)
;; (substituir-posicao 0 (linha 0 (tabuleiro-teste)) T)
;; (T 25 54 89 21 8 36 14 41 96)
(defun substituir-posicao (index lista &optional (val NIL))
"Função que recebe um índice, uma lista e um valor (por default o valor é NIL) e
substitui pelo valor pretendido nessa posição"  
  (cond 
    ( (null lista) '())
    ( (= index 0) (cons val (cdr lista)))
    (t (cons (car lista) (substituir-posicao (1- index) (cdr lista) val)))
  )

)

;;  (substituir 0 0 (tabuleiro-teste) T)
(defun substituir (lin col tabuleiro &optional (val NIL))  
"Função que recebe dois índices, o tabuleiro e um valor (por default o valor é NIL). A
função deverá retornar o tabuleiro com a célula substituída pelo valor pretendido"
  (cond 
    ( (null tabuleiro) '())
    ( (= lin 0) (cons (substituir-posicao col (linha lin tabuleiro) val ) (cdr tabuleiro)))
    (t (cons (car tabuleiro) (substituir (1- lin) col (cdr tabuleiro) val)))
  
  )
  
  
)



;; (posicao-cavalo (tabuleiro-teste))
;; (posicao-cavalo (tabuleiro-jogado))
(defun posicao-cavalo (board &optional (row 0) (column 0))
"Função que recebe o tabuleiro e devolve a posição (i j) em que se encontra o cavalo."
  (cond
    ((eq (nth column (car board)) T) (list row column))
    ((<= (length (car board)) column) (posicao-cavalo (cdr board) (1+  row)))
    ((car board) (posicao-cavalo board row (1+ column)))
    (T NIL)

  )
)
  

;;operadores 
(defun operador-1 (tabuleiro)
    "mover o cavalo 2 casas para baixo e uma para a esquerda"
    (let (
          (lin (car (posicao-cavalo tabuleiro)))
          (col (cadr (posicao-cavalo tabuleiro)))
         )
        
        (cond
         ((eq (celula (+ lin 2) (1- col) tabuleiro) NIL) NIL)
         (t (substituir (+ lin 2) (1- col) (substituir lin col tabuleiro) T))
        )
    )
    
    )


(defun operador-2 (tabuleiro)
    "mover o cavalo 2 casas para baixo e uma para a direita"
    (let (
          (lin (car (posicao-cavalo tabuleiro)))
          (col (cadr (posicao-cavalo tabuleiro)))
         )
        
        (cond
         ((eq (celula (+ lin 2) (1+ col) tabuleiro) NIL) NIL)
         (t (substituir (+ lin 2) (1+ col) (substituir lin col tabuleiro) T))
        )
    )
    
    )


(defun operador-3 (tabuleiro)
    "mover o cavalo 1 casa para baixo e duas para a direita"
    (let (
          (lin (car (posicao-cavalo tabuleiro)))
          (col (cadr (posicao-cavalo tabuleiro)))
         )
        
        (cond
         ((eq (celula (1+ lin) (+ col 2) tabuleiro) NIL) NIL)
         (t (substituir (1+ lin) (+ col 2) (substituir lin col tabuleiro) T))
        )
    )
    
    )


(defun operador-4 (tabuleiro)
    "mover o cavalo 1 casa para cima e duas para a direita"
    (let (
          (lin (car (posicao-cavalo tabuleiro)))
          (col (cadr (posicao-cavalo tabuleiro)))
         )
        
        (cond
         ((eq (celula (1- lin) (+ col 2) tabuleiro) NIL) NIL)
         (t (substituir (1- lin) (+ col 2) (substituir lin col tabuleiro) T))
        )
    )
    
    )

(defun operador-5 (tabuleiro)
    "mover o cavalo 2 casa para cima e 1 para a direita"
    (let (
          (lin (car (posicao-cavalo tabuleiro)))
          (col (cadr (posicao-cavalo tabuleiro)))
         )
        
        (cond
         ((eq (celula (+ lin 2) (1+ col) tabuleiro) NIL) NIL)
         (t (substituir (+ lin 2) (1+ col) (substituir lin col tabuleiro) T))
        )
    )
    
    )

(defun operador-6 (tabuleiro)
    "mover o cavalo 2 casa para cima e 1 para a esquerda"
    (let (
          (lin (car (posicao-cavalo tabuleiro)))
          (col (cadr (posicao-cavalo tabuleiro)))
         )
        
        (cond
         ((eq (celula (+ lin 2) (1- col) tabuleiro) NIL) NIL)
         (t (substituir (+ lin 2) (1- col) (substituir lin col tabuleiro) T))
        )
    )
    
    )

(defun operador-7 (tabuleiro)
    "mover o cavalo 1 casa para cima e 2 para a esquerda"
    (let (
          (lin (car (posicao-cavalo tabuleiro)))
          (col (cadr (posicao-cavalo tabuleiro)))
         )
        
        (cond
         ((eq (celula (1+ lin) (- col 2) tabuleiro) NIL) NIL)
         (t (substituir (1+ lin) (- col 2) (substituir lin col tabuleiro) T))
        )
    )
    
    )


(defun operador-8 (tabuleiro)
    "mover o cavalo 1 casa para baixo e 2 para a esquerda"
    (let (
          (lin (car (posicao-cavalo tabuleiro)))
          (col (cadr (posicao-cavalo tabuleiro)))
         )
        
        (cond
         ((eq (celula (1- lin) (- col 2) tabuleiro) NIL) NIL)
         (t (substituir (1- lin) (- col 2) (substituir lin col tabuleiro) T))
        )
    )
    
    )

;; (funcall (car (lista-operadores)) (tabuleiro-jogado))
;; (print-tabuleiro (funcall (nth 1 (lista-operadores)) (tabuleiro-jogado)))
(defun lista-operadores ()
    (list 'operador-1 'operador-2 'operador-3 'operador-4 'operador-5 'operador-6 'operador-7 'operador-8)
  )

;; (print-tabuleiro (tabuleiro-teste))
(defun print-tabuleiro (tabuleiro &optional (stream t))
"Formata o tabuleiro"
    (not (null (mapcar #'(lambda (l)
        (format stream "~%~t~t ~a" l)) tabuleiro))
    )
    (format t "~%")
)
