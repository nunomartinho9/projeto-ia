(load "puzzle.lisp")

(defun bfs-recursivo-aux (abertos fechados expandir-nos)
  (cond
   ((null abertos) '())
   (t
     (let* ((no-atual (car abertos))
            (novos-fechados (append fechados (list no-atual)))
            (sucessores (funcall expandir-nos no-atual))
            (novos-abertos (append (cdr abertos) sucessores)))
       (if (verificar-solucao no-atual)
           (caminho-solucao no-atual)
           (bfs-recursivo-aux novos-abertos novos-fechados expandir-nos))))))


;;(bfs-recursivo (tabuleiro-jogado) 100 'usar-operadores 'calcular-pontos 'posicionar-cavalo)
;;(bfs-recursivo (tabuleiro-teste) 100 'usar-operadores 'calcular-pontos 'posicionar-cavalo)
(defun bfs-recursivo (tabuleiro pontos-objetivo expandir-nos fn-calcular-pontos fn-primeira-jogada)
  "Algoritmo BFS recursivo para resolver o problema do cavalo."

  (let* ((no-inicial (criar-no-inicial tabuleiro pontos-objetivo))
         (primeiro-sucessor (gerar-primeiro-sucessor no-inicial fn-primeira-jogada fn-calcular-pontos))
         (abertos (list primeiro-sucessor)))
    (bfs-recursivo-aux abertos '() (lambda (no) (gerar-sucessores no expandir-nos fn-calcular-pontos)))))


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


;; ============= NOS =============
;; <no>::= (<tabuleiro> <pai> <pontos-objetivo> <pontos-atual> <profundidade> <>)
(defun no-teste ()
  '(((nil 05 nil nil nil 15 nil nil nil T)
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
    25
    0))


(defun criar-no (tabuleiro &optional (pai nil) (pontos-obj 0) (pontos-atual 0) (profundidade 0))
  "Recebe um tabuleiro, e apartir dele cria um nó com a estrutura definida."
  (list tabuleiro pai pontos-obj pontos-atual profundidade))

(defun criar-no-inicial (tabuleiro pontos-objetivo)
  "Recebe o tabuleiro inicial e os pontos objetivo e cria o primeiro nó."
  (criar-no tabuleiro nil pontos-objetivo))

(defun gerar-primeiro-sucessor (no fn-primeira-jogada fn-calcular-pontos)

  (let ((prox-tabuleiro (funcall fn-primeira-jogada (no-tabuleiro no))))

    (criar-no prox-tabuleiro no (no-pontos-final no) (funcall fn-calcular-pontos (no-pontos-atual no) (no-tabuleiro no) prox-tabuleiro) 1)))

;; (gerar-sucessores (no-teste) 'usar-operadores 'calcular-pontos)
(defun gerar-sucessores (no-atual fn-expandir-no fn-calcular-pontos)
  "Recebe um no e a função de expansão de nos, 
  (a função passada normalmente vai ser a usar-operadores que irá gerar uma lista das próximas jogadas)
  depois essa lista de tabuleiros será convertida para uma lista de nós."
  (mapcar #'(lambda (tab)
              (criar-no tab no-atual (no-pontos-final no-atual) (funcall fn-calcular-pontos (no-pontos-atual no-atual) (no-tabuleiro no-atual) tab) (+ 1 (no-profundidade no-atual)))) (funcall fn-expandir-no (no-tabuleiro no-atual))))


(defun gerar-sucessores-depu (no-atual fn-expandir-no fn-calcular-pontos)
  "Recebe um no e a função de expansão de nos, 
  (a função passada normalmente vai ser a usar-operadores que irá gerar uma lista das próximas jogadas)
  depois essa lista de tabuleiros será convertida para uma lista de nós."
  (let* ((tabuleiros-expandidos (funcall fn-expandir-no (no-tabuleiro no-atual)))
         (nos-expandidos (mapcar #'(lambda (tab)
                                     (let ((novo-no (criar-no tab no-atual
                                                              (no-pontos-final no-atual)
                                                              (funcall fn-calcular-pontos (no-pontos-atual no-atual) (no-tabuleiro no-atual) tab)
                                                              (+ 1 (no-profundidade no-atual)))))
                                       (format t "Novo Nó: ~A~%" novo-no) ;; Adicione esta linha
                                       novo-no))
                                 tabuleiros-expandidos)))

    nos-expandidos))





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


#|
(defun caminho-solucao (no)
  "Devolve uma lista de nos do no inicial ate ao no da solucao."
  (if (null (no-pai no))
      (list (list (no-tabuleiro no) (no-pontos-final no) (no-pontos-atual no) (no-profundidade no)))
      (append (caminho-solucao (no-pai no)) (list (list (no-tabuleiro no) (no-pontos-final no) (no-pontos-atual no) (no-profundidade no))))))

|#


;; ============= SELETORES =============

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

;; ============ AINDA POR TESTAR NAO MEXER AQUI EM BAIXO ============
;; ============ MEDIDAS DE DESEMPENHO ============

;; <solucao>::= (<caminho-solucao> <n-abertos> <n-fechados>)
;; <solucao-a*>::= (<caminho-solucao> <n-abertos> <n-fechados> <n-nos-expandidos>)

;; fator de ramificação média
(defun fator-ramificacao-media (lista &optional (L (tamanho-solucao lista)) (valor-T (num-nos-gerados lista)) (B-min 0) (B-max valor-T) (margem 0.1))
  "Retorna o fator de ramificacao media (metodo bisseccao)"
  (let ((B-avg (/ (+ B-min B-max) 2)))
    (cond ((< (- B-max B-min) margem) (/ (+ B-max B-min) 2))
          ((< (aux-ramificacao B-avg L valor-T) 0) (fator-ramificacao-media lista L valor-T B-avg B-max margem))
          (T (fator-ramificacao-media lista L valor-T B-min B-avg margem)))))

;; B + B^2 + ... + B^L = T
(defun aux-ramificacao (B L valor-T)
  (cond
   ((= 1 L) (- B valor-T))
   (T (+ (expt B L) (aux-ramificacao B (- L 1) valor-T)))))

(defun tamanho-solucao (lista)
  "Retorna o tamanho da solucao"
  (length (car lista)))

(defun num-nos-gerados (lista)
  "Retorna o numero de nos gerados"
  (+ (second lista) (third lista)))

(defun num-nos-expandidos (lista)
  "Retorna o numero de nos expandidos"
  (third lista))

(defun num-nos-expandidos-a* (lista)
  "Retorna o numero de nos expandidos (a*)"
  (fourth lista))

(defun penetrancia (lista)
  "Calcula a penetrancia"
  (/ (length (car lista)) (num-nos-gerados lista)))

(defun no-solucao (lista)
  "Retorna o no solucao"
  (nth (1- (length (car lista))) (car lista)))