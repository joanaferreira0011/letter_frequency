; vetor com as frequencias
(define frequenciastabeladas
  (vector
   (cons #\E 12.02)
   (cons #\T 9.10)
   (cons #\A 8.12)
   (cons #\O 7.68)
   (cons #\I 7.31)
   (cons #\N 6.95)
   (cons #\S 6.28)
   (cons #\R 6.02)
   (cons #\H 5.92)
   (cons #\D 4.32)
   (cons #\L 3.98)
   (cons #\U 2.88)
   (cons #\C 2.71)
   (cons #\M 2.61)
   (cons #\F 2.30)
   (cons #\Y 2.11)
   (cons #\W 2.09)
   (cons #\G 2.03)
   (cons #\P 1.82)
   (cons #\B 1.49)
   (cons #\V 1.11)
   (cons #\K 0.69)
   (cons #\X 0.17)
   (cons #\Q 0.11)
   (cons #\J 0.10)
   (cons #\Z 0.07)))

(define frequenciastabeladasport
  (vector
   (cons #\E 12.57)
   (cons #\T 4.34)
   (cons #\A 14.63)
   (cons #\O 10.73)
   (cons #\I 6.18)
   (cons #\N 5.05)
   (cons #\S 7.81)
   (cons #\R 6.53)
   (cons #\H 1.28)
   (cons #\D 4.99)
   (cons #\L 2.78)
   (cons #\U 4.63)
   (cons #\C 3.88)
   (cons #\M 4.74)
   (cons #\F 1.02)
   (cons #\Y 0.01)
   (cons #\W 0.01)
   (cons #\G 1.30)
   (cons #\P 2.52)
   (cons #\B 1.04)
   (cons #\V 1.67)
   (cons #\K 0.02)
   (cons #\X 0.21)
   (cons #\Q 1.20)
   (cons #\J 0.40)
   (cons #\Z 0.47)))
   
  

;o programa que me vai lixar a cabeca
(define alfabeto (make-vector 36))

(define letrasusadas (make-vector 26))


;numero de letras do texto
(define letrassemespaco
  (lambda (texto i num)
    (if (= i (string-length texto))
        num
        (if (not (equal? (string-ref texto i) #\space))
            (letrassemespaco texto (add1 i) (add1 num))
            (letrassemespaco texto (add1 i) num)))))

;frequencia letra
(define frequencialetra
  (lambda (letra texto i numero)
    (if  (= i (string-length texto))
        (* 100 (/ numero (letrassemespaco texto 0 0)))
        (if (equal? (string-ref texto i) letra)
            (frequencialetra letra texto (add1 i) (add1 numero))
            (frequencialetra letra texto (add1 i) numero)))))
            
;altera vetor
(define procuravetor
  (lambda (vetor simbolo i texto)
    (if (not (= i (vector-length vetor)))
        (if (pair? (vector-ref vetor i))
            (if (not (equal? (car (vector-ref vetor i)) simbolo))
                (procuravetor vetor simbolo (add1 i) texto))
            (vector-set! vetor i (cons simbolo (frequencialetra simbolo texto 0 0)))))))

;percorre a lista    
(define contaletras
  (lambda (textoencri)
    (letrec ((aux 
             (lambda (itexto)
               (if (not (= itexto (string-length textoencri)))
                   (if (equal? (string-ref textoencri itexto) #\space)
                       (aux (add1 itexto))
                       (begin (procuravetor alfabeto (string-ref textoencri itexto) 0 textoencri)
                          (aux (add1 itexto))))))))
     (aux 0))))

;substituir
(define substituir
  (lambda (texto letrai letraf i)
    (if (not (= i (string-length texto)))
        (if (equal (sting-ref texto i) letrai)
            (begin (string-set! texto i letraf)
                   (substituir texto letrai letraf (add1 i)))
            (substituir texto letrai letraf (add1 i))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; QUAL SUBSTITUIR? ;;;;;;;,,,,
;;;;;;;;;;;;;;;;;;;;;;;;~


;criar lista com diferencas
(define diferencas
  (lambda (par)
    (map  (lambda (x) (cons (car x) (abs (- (cdr x) (cdr par))))) (vector->list frequenciastabeladas))))
            
        
(define textoencriptado (list->string (map char-upcase (string->list (read-line)))))
(contaletras textoencriptado)
alfabeto
(diferencas (vector-ref alfabeto 5))



;lista 1 palavras
(define umapalavra
  (lambda (texto)
    (letrec ((criar-lista-letras
              (lambda (i lista)    
                (if (= i (string-length texto))
                    lista
                    ((if (and (equal? (string-ref texto (sub1 i) #\space)) (equal? (string-ref texto (add1 i) #\space))) (not (member (string-ref texto i)) lista))
                        (criar-lista-letras (add1 i) (cons (string-ref texto i) ))
                        (criar-lista-letras (add1 i) lista))))))
             (criar-lista-letras 0 '()))))
                    
                    