;; ============= ESTRUTURAS =============
;; <no>::= (<tabuleiro> <pai> <pontos-objetivo> <pontos-atual> <profundidade> <>)
;; <solucao>::= (<caminho-solucao> <n-abertos> <n-fechados>)
;; <solucao-a*>::= (<caminho-solucao> <n-abertos> <n-fechados> <n-nos-expandidos>)

;; ============= ALGORITMOS =============

;;(bfs-recursivo (tabuleiro-jogado) 100 'usar-operadores 'calcular-pontos 'posicionar-cavalo)
;;(bfs-recursivo (tabuleiro-teste) 100 'usar-operadores 'calcular-pontos 'posicionar-cavalo)
(defun bfs-recursivo (tabuleiro pontos-objetivo expandir-nos fn-calcular-pontos fn-pos-cavalo tabuleiros-cavalo-inicial)
  "Algoritmo BFS recursivo para resolver o problema do cavalo."

  (let* ((no-inicial (criar-no-inicial tabuleiro pontos-objetivo))
         (primeiros-sucressores
          (if (funcall fn-pos-cavalo tabuleiro) (list no-inicial) (gerar-sucessores no-inicial tabuleiros-cavalo-inicial fn-calcular-pontos)))
         (abertos primeiros-sucressores))
    (bfs-recursivo-aux abertos '() (lambda (no) (gerar-sucessores no expandir-nos fn-calcular-pontos)))))

#|
;; BFS ITERATIVO
(defun bfs-iterativo (tabuleiro pontos-objetivo expandir-nos fn-calcular-pontos fn-primeira-jogada)
  "Algoritmo BFS iterativo para resolver o problema do cavalo."
  (let* ((no-inicial (criar-no-inicial tabuleiro pontos-objetivo))
         (primeiro-sucessor (gerar-primeiro-sucessor no-inicial fn-primeira-jogada fn-calcular-pontos))
         (abertos (list primeiro-sucessor))
         (fechados '()))

    (loop until (null abertos)
          do
          (let ((no-atual (pop abertos)))
            (setq fechados (append fechados (list no-atual)))
            (if (verificar-solucao no-atual)
                (return (caminho-solucao no-atual)))
            (setq abertos (append abertos (gerar-sucessores no-atual expandir-nos fn-calcular-pontos)))))))|#

;; DFS RECURSIVO
(defun dfs-recursivo (tabuleiro pontos-objetivo expandir-nos fn-calcular-pontos fn-pos-cavalo tabuleiros-cavalo-inicial &optional (d 20))
  "Algoritmo DFS recursivo para resolver o problema do cavalo."

  (let* ((no-inicial (criar-no-inicial tabuleiro pontos-objetivo))
         (primeiros-sucressores
          (if (funcall fn-pos-cavalo tabuleiro) (list no-inicial) (gerar-sucessores no-inicial tabuleiros-cavalo-inicial fn-calcular-pontos)))
         (abertos primeiros-sucressores))
    (dfs-recursivo-aux abertos '() (lambda (no) (gerar-sucessores no expandir-nos fn-calcular-pontos)) d)))


;; ============= AUX ALGORITMOS =============
(defun bfs-recursivo-aux (abertos fechados expandir-nos)
  (cond
   ((null abertos) '())
   (t
     (let* ((no-atual (car abertos))
            (novos-fechados (append fechados (list no-atual)))
            (sucessores (funcall expandir-nos no-atual))
            (novos-abertos (append (cdr abertos) sucessores)))
       (if (verificar-solucao no-atual)
           (list (caminho-solucao no-atual) (length abertos) (length fechados))
           (bfs-recursivo-aux novos-abertos novos-fechados expandir-nos))))))

(defun dfs-recursivo-aux (abertos fechados expandir-nos d)
  (cond
   ((null abertos) '())
   (t
     (let* ((no-atual (car abertos))
            (novos-fechados (append fechados (list no-atual)))
            (sucessores (funcall expandir-nos no-atual))
            (novos-abertos (append (cdr abertos) sucessores)))
       (cond
        ((verificar-solucao no-atual) (list (caminho-solucao no-atual) (length abertos) (length fechados)))
        ((>= (no-profundidade no-atual) d) (dfs-recursivo-aux (cdr abertos) novos-fechados expandir-nos d))
        (t (dfs-recursivo-aux novos-abertos novos-fechados expandir-nos d)))))))
#|
  (if (verificar-solucao no-atual)
           (list (caminho-solucao no-atual) (length abertos) (length fechados))
           (bfs-recursivo-aux novos-abertos novos-fechados expandir-nos))
|#
#|Exemplo de uso:
(let ((resultado (bfs-iterativo (tabuleiro-jogado) 100 'usar-operadores 'calcular-pontos 'posicionar-cavalo)))
  (if resultado
      (print resultado)
      (print "Sem solução")))|#


;; ============= AUXILIARES =============


;; funcao de verificar se o no e a solucao
(defun verificar-solucao (no-atual)
  "Função que recebe o no atual e verifica se este é um nó solução. 
  Se a pontuação for maior ou igual que a pontuação desejada é devolvido T, caso contrário NIL."
  (cond
   ((>= (no-pontos-atual no-atual) (no-pontos-final no-atual)) T)
   (t NIL)))

(defun caminho-solucao (no)
  "Devolve uma lista de nos do no inicial ate ao no da solucao."
  (if (null (no-pai no))
      (list (list (no-tabuleiro no)
                  (no-pontos-final no)
                  (no-pontos-atual no)
                  (no-profundidade no)))
      (cons (list (no-tabuleiro no)
                  (no-pontos-final no)
                  (no-pontos-atual no)
                  (no-profundidade no))
            (caminho-solucao (no-pai no)))))


;; ============= NOS =============
;; <no>::= (<tabuleiro> <pai> <pontos-objetivo> <pontos-atual> <profundidade> <>)
(defun no-teste ()
  '(((nil 05 nil nil nil 15 nil nil nil 25)
     (nil nil nil 06 nil nil nil 16 nil nil)
     (nil 04 nil nil nil 14 nil nil nil 24)
     (nil nil nil 07 nil nil nil 17 nil nil)
     (nil 03 nil nil nil 13 nil nil nil 23)
     (nil nil nil 08 nil nil nil 18 nil nil)
     (nil 02 nil nil nil 12 nil nil nil 22)
     (nil nil nil 09 nil nil nil 19 nil nil)
     (nil 01 nil nil nil 11 nil nil nil 21)
     (nil nil nil 10 nil nil nil 20 nil nil))

    NIL
    300
    0
    0))


(defun criar-no (tabuleiro &optional (pai nil) (pontos-obj 0) (pontos-atual 0) (profundidade 0))
  "Recebe um tabuleiro, e apartir dele cria um nó com a estrutura definida."
  (list tabuleiro pai pontos-obj pontos-atual profundidade))

(defun criar-no-inicial (tabuleiro pontos-objetivo)
  "Recebe o tabuleiro inicial e os pontos objetivo e cria o primeiro nó."
  (criar-no tabuleiro nil pontos-objetivo))


;; (gerar-sucessores (no-teste) 'usar-operadores 'calcular-pontos)
(defun gerar-sucessores (no-atual fn-expandir-no fn-calcular-pontos)
  "Recebe um no e a função de expansão de nos, 
  (a função passada normalmente vai ser a usar-operadores que irá gerar uma lista das próximas jogadas)
  depois essa lista de tabuleiros será convertida para uma lista de nós."
  (mapcar #'(lambda (tab)
              (criar-no tab no-atual (no-pontos-final no-atual) (funcall fn-calcular-pontos (no-pontos-atual no-atual) (no-tabuleiro no-atual) tab) (+ 1 (no-profundidade no-atual)))) (funcall fn-expandir-no (no-tabuleiro no-atual))))


;; ============= SELETORES =============

;; <caminho-solucao>::= (lista (<tabuleiro> <pontos-obj> <pontos-atual> <profundidade>) ... )
;; <solucao>::= (<caminho-solucao> <n-abertos> <n-fechados>)
;; <solucao-a*>::= (<caminho-solucao> <n-abertos> <n-fechados> <n-nos-expandidos>)

;; NO
(defun no-tabuleiro (no)
  "retorna o tabuleiro do nó"
  (first no))

(defun no-pai (no)
  "retorna o nó pai do nó recebido"
  (second no))

(defun no-pontos-final (no)
  "retorna os pontos objetivo do nó"
  (third no))

(defun no-pontos-atual (no)
  "retorna a pontuação atual"
  (fourth no))

(defun no-profundidade (no)
  "retona a profundidade atual do nó"
  (fifth no))

;; SOLUCAO
(defun solucao-nos (solucao)
  "Buscar o caminho-solucao (lista de nos desde o inicial até ao no-solucao)"
  (first solucao))

(defun solucao-abertos (solucao)
  "Buscar o numero de abertos da solucao"
  (second solucao))

(defun solucao-fechados (solucao)
  "Buscar o numero de fechados/expandidos da solucao"
  (third solucao))

;; CAMINHO SOLUCAO
(defun no-caminho-solucao (index solucao)
  "Retorna um dos nós do caminho-solução no index recebido (começa no 0)."
  (if (and (>= index 0) (< index (tamanho-solucao solucao)))
      (nth index (solucao-nos solucao))
      NIL))


(defun no-caminho-solucao-primeiro (solucao)
  "Buscar o primeiro NO no caminho-solucao. (este NO deve ser o no solucao)"
  (no-caminho-solucao 0 solucao))

(defun no-caminho-solucao-ultimo (solucao)
  "Buscar o ultimo NO no caminho-solucao. (este NO deve ser o no inicial)"
  (no-caminho-solucao (1- (tamanho-solucao solucao)) solucao))

;;(solucao-no-tabuleiro (no-caminho-solucao-primeiro solucao-inteira))

(defun solucao-no-pontos-obj (solucao-no)
  "retorna os pontos objetivo do nó (da nova estrutura de nos que está no caminho-solucao)"
  (second solucao-no))

(defun solucao-no-pontos-atual (solucao-no)
  "retorna a pontuação atual "
  (third solucao-no))

(defun solucao-no-profundidade (solucao-no)
  "retona a profundidade atual do nó (da nova estrutura de nos que está no caminho-solucao)"
  (fourth solucao-no))

(defun tamanho-solucao (solucao)
  "Retorna o tamanho da solucao"
  (length (solucao-nos solucao)))

(defun num-nos-gerados (solucao)
  "Retorna o numero de nos gerados"
  (+ (solucao-abertos solucao) (solucao-fechados solucao)))

(defun num-nos-expandidos-a* (solucao)
  "Retorna o numero de nos expandidos (a*)"
  (fourth solucao))


;; ============ MEDIDAS DE DESEMPENHO ============

;; fator de ramificação média
(defun fator-ramificacao-media (lista &optional (L (tamanho-solucao lista)) (valor-T (num-nos-gerados lista)) (B-min 0) (B-max valor-T) (margem 0.1))
  "Retorna o fator de ramificacao media (metodo bisseccao)"
  (float (let ((B-avg (/ (+ B-min B-max) 2)))
           (cond ((< (- B-max B-min) margem) (/ (+ B-max B-min) 2))
                 ((< (aux-ramificacao B-avg L valor-T) 0) (fator-ramificacao-media lista L valor-T B-avg B-max margem))
                 (T (fator-ramificacao-media lista L valor-T B-min B-avg margem))))))

;; B + B^2 + ... + B^L = T
(defun aux-ramificacao (B L valor-T)
  (cond
   ((= 1 L) (- B valor-T))
   (T (+ (expt B L) (aux-ramificacao B (- L 1) valor-T)))))

(defun penetrancia (solucao)
  "Calcula a penetrancia"
  (float (/ (tamanho-solucao solucao) (num-nos-gerados solucao))))