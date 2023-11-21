;; Implementação dos algoritmos de procura.
;; Autores: Nuno Martinho & João Coelho.

;; <no>::= (<tabuleiro> <pai> <caixas-objetivo> <g> <h>)
;; ============ PROBLEMAS PARA TESTE ============
(defun no-teste () 
    '(
        (
		    ((0)(0))  
		    ((0)(1))    
	    )
        nil 1 0 0
     )

)

(defun problema-a ()
    '(
        (((0 0 0) (0 0 1) (0 1 1) (0 0 1))
        ((0 0 0) (0 1 1) (1 0 1) (0 1 1)))
        nil 3 0 1
     )
)

(defun problema-b ()
    '(
        (((0 0 1 0) (1 1 1 1) (0 0 1 1) (0 0 1 1) (0 0 1 1))
        ((0 0 1 1) (0 0 1 1) (1 1 1 1) (1 0 1 1) (0 1 1 1)))
        nil 7 0 1
     )
)

(defun problema-c ()
    '(
        (((0 0 1 0) (1 0 1 1) (0 0 1 1) (0 0 1 1) (0 0 1 1))
        ((0 0 1 1) (0 0 1 1) (0 0 1 1) (1 0 1 1) (0 1 1 1)))
        nil 10 0 1
     )
)

(defun problema-d ()
    '(
        (((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0))
        ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0)))
        nil 10 0 1
     )
)

(defun problema-e ()
    '(
        (((0 0 0 1 0 0) (0 0 0 1 1 1) (1 1 1 1 1 0) (0 0 0 1 1 0) (0 0 0 1 1 0) (0 0 1 1 1 1) (0 0 1 1 1 1))
        ((0 0 0 1 1 1) (0 1 0 0 1 1) (0 1 1 0 1 1) (0 0 1 1 0 0) (1 0 1 0 1 0) (0 0 1 1 0 0) (0 1 1 1 1 1)))
        nil 20 0 1
     )
)

(defun problema-f ()
    '(
        (((0 0 0 0 0 0 0) (0 0 0 0 0 0 0) (0 0 0 0 0 0 0) (0 0 0 0 0 0 0) (0 1 0 0 0 0 0) (0 1 0 0 0 0 0) (0 0 0 0 0 0 0) (0 0 0 0 0 0 0))
        ((0 0 0 0 0 0 0) (0 0 0 0 1 0 0) (0 0 0 0 0 0 0) (0 0 0 0 0 0 0) (0 0 0 0 0 0 0) (0 0 0 0 0 0 0) (0 0 0 0 0 0 0) (0 0 0 0 0 0 0)))
        nil 35 0 1
     )
)


;; ============ ALGORITMOS DE PROCURA ============
;; o no inicial vai na lista de abertos


;; (bfs 'expandir-no (list (no-teste)))
;; (((((0) (0)) ((0) (1))) (((1) (0)) ((0) (1))) (((1) (1)) ((0) (1))) (((1) (1)) ((1) (1)))) 1 7)
(defun bfs (fnExpandir abertos &optional (fechados '()))
    "Algoritmo de procura em largura primeiro: Breadth-First-Search."
    (cond 
        ( (= (length abertos) 0) NIL)
        (T
            (let*
                (
                    (no-atual (car abertos))
                    (sucessores (funcall fnExpandir no-atual))
                )
                ;;verificar se ha solucao
                (if (verificar-solucao no-atual sucessores)
                    (list (get-caminho-solucao no-atual) (length abertos) (length fechados))
                    (bfs 
                        fnExpandir 
                        (append (cdr abertos) (remover-nil (remover-duplicados (remover-duplicados sucessores abertos) fechados))  ) 
                        (append fechados (list no-atual))
                    )
                )
            )
        )
    )
)

;; (dfs 'expandir-no 3 (list (no-teste)))
;; (((((0) (0)) ((0) (1))) (((1) (0)) ((0) (1))) (((1) (1)) ((0) (1))) (((1) (1)) ((1) (1)))) 4 3)
(defun dfs (fnExpandir maxProfundidade abertos &optional (fechados '()))   
   "Algoritmo de proucra em profundidade primeiro: Depth-First-Search."
    (cond 
        ( (= (length abertos) 0) NIL)
        ( (> (get-no-g (car abertos) ) maxProfundidade) (dfs fnExpandir maxProfundidade (cdr abertos) (append fechados (list (car abertos)))))
        (T
            (let* 
                (
                    (no-atual (car abertos))
                    (sucessores (funcall fnExpandir no-atual))
                )
                (if (verificar-solucao no-atual sucessores) 
                    (list (get-caminho-solucao no-atual) (length abertos) (length fechados))
                    (dfs 
                        fnExpandir 
                        maxProfundidade 
                        (append sucessores (cdr abertos))
                        (append fechados (list no-atual))
                    )
                )
            )
        )
    )
)


;; (a* 'expandir-no-a* 'heuristica-base (list (no-teste)))
;; (((((0) (0)) ((0) (1))) (((0) (0)) ((1) (1))) (((0) (1)) ((1) (1))) (((1) (1)) ((1) (1)))) 2 5 5)
(defun a* (fnExpandir fnHeuristica abertos &optional (fechados '()) (numeroExpandidos 0))
    "Algoritmo A*" 
    (cond 
        ((= (length abertos) 0) NIL)
        (T
            (let*
                (
                    (no-atual (substituir '5 (get-f-mais-baixo abertos) (funcall fnHeuristica (get-f-mais-baixo abertos))) )
                    (sucessores (funcall fnExpandir no-atual fnHeuristica))
                    (novos-fechados (recalcular-fechados fechados sucessores no-atual) )    ;; recalcular f dos abertos
                    (novos-abertos (recalcular-abertos (cdr abertos) sucessores no-atual) ) ;; recalcular f dos fechados
                    (abertos-com-novos-fechados (remover-nil (append novos-abertos (remover-duplicados sucessores novos-abertos) novos-fechados))) ;;passar os novos fechados para abertos
                )
                (if (verificar-solucao no-atual sucessores)
                    (list (get-caminho-solucao no-atual) (length abertos) (length fechados) numeroExpandidos)
                    (a* fnExpandir fnHeuristica abertos-com-novos-fechados (remover-duplicados (append fechados (list no-atual)) novos-fechados) (1+ numeroExpandidos) )
                )

            )
        )
    )

)


;; ============ AUXILIARES PARA A* ============

;; (recalcular-abertos abertos sucessores pai)
;; 
(defun recalcular-abertos (abertos sucessores no-pai)
    "Recebe uma lista de nos abertos, lista de nos expandidos e o no pai
    Se algum no expandido existe em abertos, ficam o no com o menor valor de f e trocamos o pai, e retornamos os novos abertos"
    (mapcar 
        #'(lambda (no-aberto)
            (let ( (novos-abertos (recalcular-no no-aberto sucessores) ) )
                (if (null novos-abertos) 
                    no-aberto 
                    (trocar-no-pai (car novos-abertos) no-pai)
                )
            )
          ) abertos)
)

;; (recalcular-fechados abertos sucessores pai)
;; 
(defun recalcular-fechados (fechados sucessores no-pai)
    "Recebe uma lista de nos fechados, lista de nos expandidos e o no pai
    Se algum no expandido existe em fechados, ficam os nos com o menor valor de f e trocamos o pai, 
    e retornamos os novos fechados para passarem em abertos"
    (mapcar 
        #'(lambda (no-fechado)
            (let ( (novos-fechados (recalcular-no no-fechado sucessores) ) )
                (if (null novos-fechados) 
                    NIL 
                    (trocar-no-pai (car novos-fechados) no-pai)
                )
            )
          ) fechados)
)

;; (recalcular-no no sucessores)
;; 
(defun recalcular-no (no sucessores)
    "Se o no dado existir na lista de nos expandidos, altera-se o no da lista com o menor valor de f entre os 2."
    (remover-nil
        (mapcar
            #'(lambda(no-sucessor)
                (if (equal (get-no-estado no) (get-no-estado no-sucessor)) 
                    (if (<= (calcular-no-f no-sucessor) (calcular-no-f no) )
                        (novo-valor-f no (get-no-g no-sucessor) (get-no-h no-sucessor))
                        nil
                    )
                    nil
                )
            
            ) sucessores)
    )
)

;; (novo-valor-f no novoG novoH)
;;
(defun novo-valor-f (no novoG novoH)
    "Calcula o novo valor de f de um no e devolve esse no."
    (substituir '5 (substituir '4 no novoG) novoH)
)

;; (trocar-no-pai no novoPai)
;;
(defun trocar-no-pai (no novoPai)
    "Troca o pai de um no, Devolve o no com o novo pai."
    (substituir '2 no novoPai)
)

;; ============ AUXILIARES PARA ALGORITMOS DE PROCURA ============


(defun remover-duplicados (lista1 lista2)
"Remove da lista1 os valores ja existentes na lista2"
  (if (or (null lista1) (null lista2))
      lista1
      (remover-nil (mapcar #'(lambda(elm2) (if (existe-valor elm2 lista2) NIL elm2)) lista1) ) 
  )
)

(defun existe-valor (valor lista)
 "Devolve T ou NIL se o valor existe ou nao dentro da lista."
  (eval (cons 'or (mapcar #'(lambda(elemento) (equal (get-no-estado valor) (get-no-estado elemento))) lista)))
)

(defun remover-nil (lista)
    "Remove os Nils de uma lista e a devolve."
    (apply #'append (mapcar #'(lambda(elem) (if (null elem) NIL (list elem))) lista))
)

(defun get-caminho-solucao (no)
    "Devolve uma lista de estados do no inicial ate ao no da solucao."
    (cond
        ( (null (get-no-pai no)) (list (get-no-estado no)))
        (T
           (append (get-caminho-solucao (get-no-pai no)) (list (get-no-estado no)) )
        )
    )
)

(defun verificar-solucao (no sucessores)
    "Verificacao booleana se o no dado e a solucao, Devolve T se uma das condicoes for verdade
    Caso contrario devolve NIL."
    (or
        (= (- (get-no-objetivo no) (calcular-caixas-fechadas (get-no-estado no))) 0)
        (= (length sucessores) 0)
    )
)

;; ============ SELETORES ============

(defun get-no-estado (no) 
 "Devolve o estado (tabuleiro) de um no."
    (car no)
)

(defun get-no-pai (no)
 "Devolve o no pai deste no."   
    (cadr no)
)

(defun get-no-objetivo (no)
    "Devolve o numero de caixas fechadas deste estado."
    (nth 2 no)
)

(defun get-no-g (no)
    "Devolve o g (profundidade) de um no."
    (nth 3 no)
)

(defun get-no-h (no)
    "Devolve a heuristica de um no."
    (nth 4 no)
)

(defun calcular-no-f (no)
 "Calcula o valor de f (funcao avaliacao) de um no."
    (+ (get-no-g no) (get-no-h no))
)

;; (get-f-mais-baixo '(((((1) (0)) ((0) (1))) ((((0) (0)) ((0) (1))) NIL 1 0 0) 1 1 4) ((((0) (1)) ((0) (1))) ((((0) (0)) ((0) (1))) NIL 1 0 0) 1 1 2) ((((0) (0)) ((1) (1))) ((((0) (0)) ((0) (1))) NIL 1 0 0) 1 1 1))
;; ((((0) (0)) ((1) (1))) ((((0) (0)) ((0) (1))) NIL 1 0 0) 1 1 1)
(defun get-f-mais-baixo (lista)
    "Devolve o no com o f mais baixo de uma lista."
    (cond
        ((= (length lista) 1) (car lista))
        (T
            (let 
            (
                (outro-no (get-f-mais-baixo (cdr lista)))
            )
            
                (if (< (calcular-no-f (car lista)) (calcular-no-f outro-no))
                    (car lista)
                    outro-no
                )
            )
        )
    )
)

;; ============ EXPANSAO DE NOS ============

;; (expandir-no-a* (no-teste) 'heuristica-base)
#| (((((1) (0)) ((0) (1))) ((((0) (0)) ((0) (1))) NIL 1 0 0) 1 1 1)
 ((((0) (1)) ((0) (1))) ((((0) (0)) ((0) (1))) NIL 1 0 0) 1 1 1)
((((0) (0)) ((1) (1))) ((((0) (0)) ((0) (1))) NIL 1 0 0) 1 1 1)) |#

(defun expandir-no-a* (no-atual fnHeuristica)
    "Expande o no e calcula a sua heuristica"
    (mapcar 
        #'(lambda (no) (substituir '5 no (funcall fnHeuristica no)))
        (expandir-no no-atual)
    )
)

;; (expandir-no (no-teste))
#|
 (
    ((((1) (0)) ((0) (1))) ((((0) (0)) ((0) (1))) NIL 1 0 0) 1 1 0)
    ((((0) (1)) ((0) (1))) ((((0) (0)) ((0) (1))) NIL 1 0 0) 1 1 0)
    ((((0) (0)) ((1) (1))) ((((0) (0)) ((0) (1))) NIL 1 0 0) 1 1 0)
 )
 |#
(defun expandir-no (no)
    "Expande um no e devolve os seus sucessores."
    (append (gerar-nos-horizontal no) (gerar-nos-vertical no))
)

;; (gerar-nos-horizontal (no-teste))
#|
(
  (
    (((1) (0)) ((0) (1))) 
    ((((0) (0)) ((0) (1))) NIL 1 0 0) 
    1 1 0
  )
  (
    (((0) (1)) ((0) (1))) 
    ((((0) (0)) ((0) (1))) NIL 1 0 0) 
    1 1 0
  )
)
|#

(defun gerar-nos-horizontal (no &optional (linha 1) (col 1))
    "Devolve os sucessores de um no, da parte horizontal do tabuleiro. (Começa no index 1)"
    (cond 
        ( (> col (count-colunas (get-no-estado no))) (gerar-nos-horizontal no (1+ linha)))
		( (> linha (count-linhas (get-no-estado no))) '())
        ( (= (get-arco-na-posicao (1- linha) (1- col) (get-arcos-horizontais (get-no-estado no) )) 1)  (gerar-nos-horizontal no linha (1+ col)))

        (T
            (cons 
                (criar-no (arco-horizontal linha col (get-no-estado no)) no (get-no-objetivo no) (1+ (get-no-g no))) 
                (gerar-nos-horizontal no linha (1+ col))
            )    
        )
    )
)
;; (gerar-nos-vertical (no-teste))
#|
(
  (
    (((0) (0)) ((1) (1))) 
    ((((0) (0)) ((0) (1))) NIL 1 0 0) 
    1 1 0
  )
)
 |#

(defun gerar-nos-vertical (no &optional (linha 1) (col 1))
    "Devolve os sucessores de um no, da parte vertical do tabuleiro. (Começa no index 1)"
    (cond 
        ( (> col (count-colunas (get-no-estado no))) (gerar-nos-vertical no (1+ linha)))
		( (> linha (count-linhas (get-no-estado no))) '())
        ( (= (get-arco-na-posicao (1- linha) (1- col) (get-arcos-verticais (get-no-estado no) )) 1)  (gerar-nos-vertical no linha (1+ col)))

        (T
            (cons 
                (criar-no (arco-vertical col linha (get-no-estado no)) no (get-no-objetivo no) (1+ (get-no-g no))) 
                (gerar-nos-vertical no linha (1+ col))
            )    
        )
    )
)

;; (criar-no (test-board) nil 1)
;;( ( ((0)(0))  ((0)(1)) ) nil 1 0 0 )
(defun criar-no (tabuleiro pai caixas-objetivo &optional (g 0) (h 0))
"Constroi a estrutura do no."
  (list tabuleiro pai caixas-objetivo g h)
)

;; ============ HEURISTICAS ============

;; (heuristica-base '((((0)(0)) ((0)(1))) nil 1 0 0))
;; 1
(defun heuristica-base (no)
 "Heuristica dada no enunciado: h(x) = o(x) _ c(x) : o(x): objetivo de caixas do tabuleiro, c(x): numero de caixas fechadas"
  (- (get-no-objetivo no) (calcular-caixas-fechadas (get-no-estado no)) )  

)

(defun heuristica-top (no)
    "Heuristica criada pelos autores. NAO ESTA FEITO."
    (* (- (get-no-objetivo no) (calcular-caixas-fechadas (get-no-estado no)) )  2)
)

;; ============ MEDIDAS DE DESEMPENHO ============

;; <solucao>::= (<caminho-solucao> <n-abertos> <n-fechados>)
;; <solucao-a*>::= (<caminho-solucao> <n-abertos> <n-fechados> <n-nos-expandidos>)

;; fator de ramificação média
(defun fator-ramificacao-media (lista &optional (L (tamanho-solucao lista)) (valor-T (num-nos-gerados lista)) (B-min 0) (B-max valor-T) (margem 0.1))
"Retorna o fator de ramificacao media (metodo bisseccao)"
    (let ((B-avg (/ (+ B-min B-max) 2)))
        (cond ((< (- B-max B-min) margem) (/ (+ B-max B-min) 2))
              ((< (aux-ramificacao B-avg L valor-T) 0) (fator-ramificacao-media lista L valor-T B-avg B-max margem))
              (T (fator-ramificacao-media lista L valor-T B-min B-avg margem))   
        )
    )
)

;; B + B^2 + ... + B^L = T
(defun aux-ramificacao (B L valor-T)
  (cond
    ((= 1 L) (- B valor-T))
    (T (+ (expt B L) (aux-ramificacao B (- L 1) valor-T)))
  )
)

(defun tamanho-solucao (lista)
"Retorna o tamanho da solucao"
    (length (car lista))
)

(defun num-nos-gerados (lista)
"Retorna o numero de nos gerados"
    (+ (second lista) (third lista))
)

(defun num-nos-expandidos (lista)
"Retorna o numero de nos expandidos"
    (third lista)
)

(defun num-nos-expandidos-a* (lista)
"Retorna o numero de nos expandidos (a*)"
    (fourth lista)
)

(defun penetrancia (lista)
"Calcula a penetrancia"
    (/ (length (car lista)) (num-nos-gerados lista))
)

(defun no-solucao (lista)
"Retorna o no solucao"
    (nth (1- (length (car lista))) (car lista))
)
