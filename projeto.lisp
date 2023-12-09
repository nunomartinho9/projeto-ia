;;  Carrega os outros ficheiros de código, 
;;  escreve e lê ficheiros, e trata da interação com o utilizador
;;  Autores: Nuno Martinho & João Coelho.




;; ============ CARREGAR FICHEIROS ============

(load "procura.lisp")
(load "puzzle.lisp")

(defun teste ()
    
    (let ((opcao (read)))
      (numberp opcao)
        ))

(defun iniciar ()
    (clear-input)
    (menu)

    (let ((opcao (read)))
        (if
            (or (not (numberp opcao)) (< opcao 1) (> opcao 3))
            (progn (format t "Escolha uma opção válida!") (iniciar))
            (ecase opcao
             
             (1 (format T "" ))
             (2 (format T "" ))
             (3 (format T "quit" ))
             
             
             )

        )
        
    )
  )

;; TODO: ISTO FICA EM LOOP TUDO BUGADo
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
    )
    
    )

(defun tabuleiros-menu (&optional (i 1) (problemas (ler-tabuleiros)))
"Mostra os tabuleiros disponíveis no menu"
    (cond ((null problemas) 
            (progn
                (format t "~%|                                |")
                (format t "~%|        0 - Voltar atras        |") 
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
;; TODO: ISTO FICA EM LOOP TUDO BUGADo

(defun opcao-tabuleiro (&optional (voltar 'iniciar))
"Recebe um tabuleiro do menu"
    (progn 
        (tabuleiros-menu)
        (let ((opcao (read)))
         (if (or (not (numberp opcao)) )
             (progn
                (format t "Escolha uma opção válida~%")
                (tabuleiros-menu)
              )
             (cond
              ((eq opcao '0) (funcall voltar))
              
              )
             
             
             )
            
        )
    )
)



;; ============= INPUT/OUTPUT =============
;; (ler-tabuleiros)
(defun ler-tabuleiros ()
"Le os tabuleiros no ficheiro problemas.dat"
    (with-open-file (stream "problemas.dat" :if-does-not-exist nil)
        (do ((result nil (cons next result))
                (next (read stream nil 'eof) (read stream nil 'eof)))
                    ((equal next 'eof) (reverse result))
        )
    )
)

(defun escolher-problema (indice &optional (lista (ler-tabuleiros)))
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
            (format t "~4t")
            (format stream "Pontos necessários: ~d" (pontuacao-problema tabuleiroEscolhido))
            (format t "~%")
            (format-tabuleiro (tabuleiro-problema tabuleiroEscolhido))
         )
        )
        
        )

    (format t "~%")
)

;; (print-tabuleiros (ler-tabuleiros))
(defun print-tabuleiros (&optional (ln 1))
   
          
        (cond
            ((> ln (length (ler-tabuleiros))) (format T "------------------------------------") )
            (t 
                (progn
                    (print-tabuleiro ln)
                    (format T "~%~%" )
                    (print-tabuleiros (1+ ln))
                
                )
             )
     )
        
        

)