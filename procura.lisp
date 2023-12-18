;; ============= ESTRUTURAS =============
;; <no>::= (<tabuleiro> <pai> <pontos-objetivo> <pontos-atual> <profundidade> <h> <f>)
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


;; DFS RECURSIVO
(defun dfs-recursivo (tabuleiro pontos-objetivo expandir-nos fn-calcular-pontos fn-pos-cavalo tabuleiros-cavalo-inicial &optional (d 20))
  "Algoritmo DFS recursivo para resolver o problema do cavalo."

  (let* ((no-inicial (criar-no-inicial tabuleiro pontos-objetivo))
         (primeiros-sucressores
          (if (funcall fn-pos-cavalo tabuleiro) (list no-inicial) (gerar-sucessores no-inicial tabuleiros-cavalo-inicial fn-calcular-pontos)))
         (abertos primeiros-sucressores))
    (dfs-recursivo-aux abertos '() (lambda (no) (gerar-sucessores no expandir-nos fn-calcular-pontos)) d)))


(defun a* (tabuleiro pontos-objetivo expandir-nos fn-calcular-pontos fn-pos-cavalo tabuleiros-cavalo-inicial fn-heuristica)

  (let* ((no-inicial(criar-no-inicial-a* tabuleiro pontos-objetivo 0 0 0 0))
         (primeiros-sucessores
          (if (funcall fn-pos-cavalo tabuleiro) (list no-inicial) (gerar-sucessores-a* no-inicial tabuleiros-cavalo-inicial fn-calcular-pontos fn-heuristica))
          )
          (abertos primeiros-sucessores)
          )
          (a*-aux2 abertos '() (lambda (no) (gerar-sucessores-a* no expandir-nos fn-calcular-pontos fn-heuristica)) fn-calcular-pontos fn-heuristica)
         )
          
    
    )
  




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


;; ============= AUXILIARES A* =============



(defun a*-aux2 (abertos fechados expandir-nos fn-calcular-pontos fn-heuristica &optional (numeroExpandidos 0))
  (if (null abertos)
      '()
      (let* (
             (no-atual (car (ordenar-por-f abertos)))
             (novos-fechados (ordenar-por-f (append fechados (list no-atual))))
             (sucessores (funcall expandir-nos no-atual))
             (fechados-para-abrir (recalcular-fechados fechados sucessores no-atual))
             (novos-abertos (recalcular-abertos (cdr abertos) sucessores no-atual) )
             (abertos-com-novos-fechados (append novos-abertos (remover-duplicados-com-maior-f sucessores novos-abertos) fechados-para-abrir))
             )
        
        (if (verificar-solucao no-atual)
            (list (caminho-solucao no-atual) (length abertos) (length fechados) numeroExpandidos)
            (a*-aux2 abertos-com-novos-fechados (remover-duplicados-com-maior-f novos-fechados fechados-para-abrir) expandir-nos fn-calcular-pontos fn-heuristica (1+ numeroExpandidos))
            )
        
        )
      
 
      )
  
  )






(defun recalcular-abertos (abertos sucessores no-pai)
    "Recebe uma lista de nos abertos, lista de nos expandidos e o no pai
    Se algum no expandido existe em abertos, ficam o no com o menor valor de f e trocamos o pai, e retornamos os novos abertos"
    (mapcar 
        #'(lambda (no-aberto)
            (let ( (novos-abertos (recalcular-no no-aberto sucessores) ) )
                (if (null novos-abertos) 
                    no-aberto 
                    (trocar-no-pai (car (ordenar-por-f novos-abertos)) no-pai)
                )
            )
          ) abertos)
  )

(defun recalcular-fechados (fechados sucessores no-pai)
    "Recebe uma lista de nos fechados, lista de nos expandidos e o no pai
    Se algum no expandido existe em fechados, ficam os nos com o menor valor de f e trocamos o pai, 
    e retornamos os novos fechados para passarem em abertos"
  (remove-if #'(lambda (x) (eq x nil)) 
    
  (mapcar 
        #'(lambda (no-fechado)
            (let ( (novos-fechados (recalcular-no no-fechado sucessores) ) )
                (if (null novos-fechados) 
                    NIL 
                    (trocar-no-pai (car (ordenar-por-f novos-fechados)) no-pai)
                )
            )
          ) fechados)
    
    )  
)

(defun trocar-no-pai (no no-pai)
  (criar-no 
    (no-tabuleiro no)
    no-pai
    (no-pontos-final no)
    (no-pontos-atual no)
    (no-profundidade no)
    (no-h no)
    (no-f no)
    
    )
  
  )

(defun recalcular-no (no sucessores)
    "Se o no dado existir na lista de nos expandidos, altera-se o no da lista com o menor valor de f entre os 2."
    (remove-if #'(lambda (x) (eq x nil)) 
        (mapcar
            #'(lambda(no-sucessor)
                (if (comparar-estados no no-sucessor) 
                    (if (<= (no-f no-sucessor) (no-f no) )
                        no-sucessor
                        nil
                    )
                    nil
                )
            
            ) sucessores)
    )
)


(defun remover-duplicados-com-maior-f (sucessores lista2)
  "Remove da sucessores os elementos que estão na lista2 se o estado for igual e o valor de F de sucessores for maior que lista2"
  (remove-if #'(lambda (x) (eq x nil)) 
      
    (mapcar 
      #'(lambda (sucessor)
          (let ((no-igual (encontrar-no sucessor lista2)))
            (if no-igual
                NIL
                sucessor
            )
            )
            
            )sucessores)
      
      )
  )


#|Exemplo de uso:
(let ((resultado (bfs-iterativo (tabuleiro-jogado) 100 'usar-operadores 'calcular-pontos 'posicionar-cavalo)))
  (if resultado
      (print resultado)
      (print "Sem solucao")))|#
(defun verificar-solucao (no-atual)
  "Funcao que recebe o no atual e verifica se este e um no solucao. 
  Se a pontuacao for maior ou igual que a pontuacao desejada e devolvido T, caso contrario NIL."
  (cond
   ((>= (no-pontos-atual no-atual) (no-pontos-final no-atual)) T)
   (t NIL)))

(defun algum-objetivo-p (sucessores)
  "Verifica se algum dos sucessores e um no objetivo."
  (some #'(lambda (sucessor) (verificar-solucao sucessor)) sucessores))

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

(defun caminho-solucao-a* (no)
  "Devolve uma lista de nos do no inicial ate ao no da solucao para A*."
  (if (null (no-pai no))
      (list no)
      (cons no (caminho-solucao-a* (no-pai no)))))

;; ============= HEURISTICAS E AVALIACAO =============

(defun heuristica-base (no &optional (fn-calcular-m 'media-casas-pontos))
  "h(x) = o(x)/m(x) : uma heuristica que privilegia visitar as casas com o maior numero de pontos.
  m(x) e a media por casa dos pontos que constam no tabuleiro x,
  o(x) e o numero de pontos que faltam para atingir o valor definido como objetivo."

  (let ((o (- (no-pontos-final no) (no-pontos-atual no)))
        (m (funcall fn-calcular-m (no-tabuleiro no)))
        )
    (/ o m)
    )

  )

(defun calcular-f (no)
  "Calcula o valor de f (funcao avaliacao) de um no."
  (+ (no-profundidade no) (no-h no)))

(defun ordenar-por-f (lista-nos)
  "Ordena uma lista de nos por ordem crescente do valor de f."
  (sort lista-nos #'(lambda (no1 no2) (< (no-f no1) (no-f no2)))))
;; ============= NOS =============

(defun no-teste ()
  '(  (
    (02   20   44   nil  nil  nil  nil  nil  nil  nil)
    (nil  nil  nil  nil  nil  nil  nil  nil  nil  nil)
    (nil  03   30   nil  nil  nil  nil  nil  nil  nil)
    (nil  nil  nil  nil  nil  nil  nil  nil  nil  nil)
    (nil  nil  nil  22   nil  nil  nil  nil  nil  nil)
    (nil  nil  nil  nil  nil  nil  nil  nil  nil  nil)
    (nil  nil  nil  nil  nil  nil  nil  nil  nil  nil)
    (nil  nil  nil  nil  nil  nil  nil  nil  nil  nil)
    (nil  nil  nil  nil  nil  nil  nil  nil  nil  nil)
    (nil  nil  nil  nil  nil  nil  nil  nil  nil  nil)
  )

    NIL
    70
    0
    0))


(defun criar-no (tabuleiro &optional (pai nil) (pontos-obj 0) (pontos-atual 0) (profundidade 0) (h 0) (f 0))
  "Recebe um tabuleiro, e apartir dele cria um no com a estrutura definida."
  (list tabuleiro pai pontos-obj pontos-atual profundidade h f))

(defun criar-no-inicial (tabuleiro pontos-objetivo)
  "Recebe o tabuleiro inicial e os pontos objetivo e cria o primeiro no."
  (criar-no tabuleiro nil pontos-objetivo 0 0 0 0))

(defun criar-no-inicial-a* (tabuleiro pontos-objetivo pontos-atual g h f)
  (criar-no tabuleiro nil pontos-objetivo pontos-atual g h f))

(defun gerar-sucessores-a* (no-atual fn-expandir-no fn-calcular-pontos fn-heuristica)
  (let* ((sucessores (gerar-sucessores no-atual fn-expandir-no fn-calcular-pontos)))
    (mapcar (lambda (no)
              
              (let ((heuristica (funcall fn-heuristica no)))
                (criar-no (no-tabuleiro no)
                          (no-pai no)
                          (no-pontos-final no)
                          (no-pontos-atual no)
                          (no-profundidade no)
                          heuristica
                          (+ (no-profundidade no) heuristica)))
            ) sucessores)
    )
  )



;; (gerar-sucessores (no-teste) 'usar-operadores 'calcular-pontos)
(defun gerar-sucessores (no-atual fn-expandir-no fn-calcular-pontos)
  "Recebe um no e a funcao de expansão de nos, 
  (a funcao passada normalmente vai ser a usar-operadores que ira gerar uma lista das proximas jogadas)
  depois essa lista de tabuleiros sera convertida para uma lista de nos."
  (mapcar #'(lambda (tab)
              (criar-no tab no-atual (no-pontos-final no-atual)
                        (funcall fn-calcular-pontos (no-pontos-atual no-atual) (no-tabuleiro no-atual) tab)
                        (+ 1 (no-profundidade no-atual))))
    (funcall fn-expandir-no (no-tabuleiro no-atual))))


;; ============= SELETORES =============

;; <caminho-solucao>::= (lista (<tabuleiro> <pontos-obj> <pontos-atual> <profundidade>) ... )
;; <solucao>::= (<caminho-solucao> <n-abertos> <n-fechados>)
;; <solucao-a*>::= (<caminho-solucao> <n-abertos> <n-fechados> <n-nos-expandidos>)

(defun encontrar-no (no lista)
  
  (cond
   ((null lista) nil)
   ((comparar-estados no (car lista)) (car lista))
   (t (encontrar-no no (cdr lista)))
   
   )
  
  )


(defun comparar-estados (no1 no2)
  (equal (no-tabuleiro no1) (no-tabuleiro no2)))



;; NO
(defun no-tabuleiro (no)
  "retorna o tabuleiro do no"
  (first no))

(defun no-pai (no)
  "retorna o nó pai do no recebido"
  (second no))

(defun no-pontos-final (no)
  "retorna os pontos objetivo do no"
  (third no))

(defun no-pontos-atual (no)
  "retorna a pontuacao atual"
  (fourth no))

(defun no-profundidade (no)
  "retona a profundidade atual do no"
  (fifth no))

(defun no-h (no)
  "Retorna o valor heuristico de um no"
  (sixth no))

;; Adicione o seguinte seletor para obter o valor de f de um nó
(defun no-f (no)
  "Retorna o valor de f de um no."
  (seventh no))


;; SOLUCAO
(defun solucao-nos (solucao)
  "Buscar o caminho-solucao (lista de nos desde o inicial ate ao no-solucao)"
  (first solucao))

(defun solucao-abertos (solucao)
  "Buscar o numero de abertos da solucao"
  (second solucao))

(defun solucao-fechados (solucao)
  "Buscar o numero de fechados/expandidos da solucao"
  (third solucao))

;; CAMINHO SOLUCAO
(defun no-caminho-solucao (index solucao)
  "Retorna um dos nos do caminho-solucao no index recebido (começa no 0)."
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
  "retorna os pontos objetivo do no (da nova estrutura de nos que esta no caminho-solucao)"
  (second solucao-no))

(defun solucao-no-pontos-atual (solucao-no)
  "retorna a pontuacao atual "
  (third solucao-no))

(defun solucao-no-profundidade (solucao-no)
  "retona a profundidade atual do no (da nova estrutura de nos que esta no caminho-solucao)"
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
