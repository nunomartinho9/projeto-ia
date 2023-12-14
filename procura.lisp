(load "puzzle.lisp")
#|
  BFS
temos uma lista de abertos e fechados
estrutura do no: <no>::= (<tabuleiro> <pai> <pontos-objetivo> <pontos-atual> <profundidade>)

1. Nó inicial => ABERTOS
2. Se ABERTOS vazia falha.
3. Remove o primeiro nó de ABERTOS (n) e 
coloca-o em FECHADOS 
4. Expande o nó n. Colocar os sucessores 
no fim de ABERTOS, colocando os 
ponteiros para n.
5. Se algum dos sucessores é um nó 
objectivo sai, e dá a solução. Caso 
contrário vai para 2.


|#

(defun bfs-recursivo (tabuleiro expandir-nos)
  "Algoritmo BFS (Busca em Largura) recursivo"
  (bfs-recursivo-aux (list (list tabuleiro nil nil 0 0)) '() expandir-nos))

(defun bfs-recursivo-aux (expandir-nos abertos &optional (fechados '()))
  
    (cond 
     ((null abertos) '()) ;; Se a lista de abertos estiver vazia, a procura terminou
     (t
         (let* (
                (no-atual (car abertos))
                (tabuleiro-atual (first no-atual))
                (sucessores (funcall expandir-nos tabuleiro-atual))
                (novos-fechados (adjoin tabuleiro-atual fechados))
                (novos-abertos (remove-if #'(lambda (no) (member (first no) (append novos-fechados abertos)))
                                    (mapcar #'(lambda (sucessor) (list sucessor no-atual)) sucessores))))
             
             (cons no-atual (bfs-recursivo-aux (cdr abertos) (append novos-fechados novos-abertos) expandir-nos)))))
    
    )



#| GONCALO



(defun bfs-recursivo-aux (abertos fechados expandir-nos)
  (cond
   ((null abertos) '()) ;; Se a lista de abertos estiver vazia, a busca terminou
   (t
    (let* ((no-atual (car abertos))
           (tabuleiro-atual (first no-atual))
           (nos-vizinhos (funcall expandir-nos tabuleiro-atual))
           (novos-fechados (adjoin tabuleiro-atual fechados))
           (novos-abertos (remove-if #'(lambda (no) (member (first no) (append novos-fechados abertos)))
                                    (mapcar #'(lambda (vizinho) (list vizinho no-atual)) nos-vizinhos))))
      (cons no-atual (bfs-recursivo-aux (cdr abertos) (append novos-fechados novos-abertos) expandir-nos))))))

(defun bfs-recursivo (tabuleiro expandir-nos)
  "Algoritmo BFS (Busca em Largura) recursivo"
  (bfs-recursivo-aux (list (list tabuleiro nil nil 0 0)) '() expandir-nos))

|#

;; ============= SELETORES =============

(defun no-tabuleiro (no)
  (first no)
  )

(defun no-pai (no)
  (second no)
  )

(defun no-pontos-final (no)
  (third no)
  )

(defun no-pontos-atual (no)
  (fourth no)
  )

(defun no-profundidade (no)
  (fifth no)
  )

;; ============= NOS =============
;; <no>::= (<tabuleiro> <pai> <pontos-objetivo> <pontos-atual> <profundidade> <>)
(defun no-teste ()
  '(
    ((NIL 25 54 89 T 8 36 14 41 96)
    (78 47 56 23 5 49 13 12 26 60)
    (0 27 17 83 34 93 74 52 45 80)
    (69 9 77 95 55 39 91 73 57 30)
    (24 15 22 86 1 11 68 79 76 72)
    (81 48 32 2 64 16 50 37 29 71)
    (99 51 6 18 53 28 7 63 10 88)
    (59 42 46 85 90 75 87 43 20 31)
    (3 61 58 44 65 82 19 4 35 62)
    (33 70 84 40 66 38 92 67 98 97))
    
    NIL
    70
    0
    0 
  )
  
  )


(defun criar-no (tabuleiro &optional (pai nil) (pontos-obj 0) (pontos-atual 0) (profundidade 0))
    
    (list tabuleiro pai pontos-obj pontos-atual profundidade)
  )

(defun criar-no-inicial (tabuleiro pontos-objetivo)
  
  (criar-no tabuleiro nil pontos-objetivo)
  )

;; (gerar-sucessores (no-teste) 'usar-operadores)
;; passar as listas de tabuleiros para lista de nos
;; (gerar-sucessores (no-teste) 'usar-operadores)
(defun gerar-sucessores (no-atual fn-expandir-no)
  
  (mapcar #'(lambda (tab)
              (criar-no tab no-atual (no-pontos-final no-atual) (calcular-pontos (no-pontos-atual no-atual) (no-tabuleiro no-atual) tab) (+ 1 (no-profundidade no-atual)))
              ) (funcall fn-expandir-no (no-tabuleiro no-atual)))
  )





;; calcular score de um no
(defun calcular-pontos (pontos-atual tabuleiro-anterior tabuleiro-novo)
  
  (let* (
         (pos (posicao-cavalo tabuleiro-novo))
         (pontos (celula (car pos) (cadr pos) tabuleiro-anterior))
         )
      (+ pontos-atual pontos)
    )
  
  )


;; gerar nos sucessores

;; funcao de verificar se o no e a solucao
(defun verificar-solucao (no-atual)
  "Função que recebe o no atual e verifica se este é um nó solução. 
  Se a pontuação for maior ou igual que a pontuação desejada é devolvido T, caso contrário NIL."
  (cond 
   ((>= (no-pontos-atual no-atual) (no-pontos-final no-atual)) T)
   (t NIL)
   )

  )

(defun caminho-solucao (no)
    "Devolve uma lista de nos do no inicial ate ao no da solucao."
    (cond
        ( (null (no-pai no)) no)
        (T
           (append (caminho-solucao (no-pai no)) no)
        )
    )
)