#lang racket
;Realizado por Rafael Perea

;Primer ejercicio
(define (segundos-tiempo numero medida)
  (cond  
      [(equal? medida 'segundos) (string-append (~a numero)  " segundo(s)")]
      [(equal? medida 'minutos) (string-append (~a (minutos numero)) " minuto(s)")]
      [(equal? medida 'horas) (string-append (~a (horas numero)) " hora(s)")]
      [(equal? medida 'dias) (string-append (~a (dias numero)) " dia(s)")]
      [(equal? medida 'semanas) (string-append (~a (semanas numero)) " semana(s)")]
      [(equal? medida 'meses) (string-append (~a (meses numero)) " mes(es)")]
      [(equal? medida 'años) (string-append (~a (años numero)) " año(s)")]
  )
)

(define (minutos segundos)
  (/ segundos 60))

(define (horas segundos)
  (/ segundos 3600))

(define (dias segundos)
  (/ (horas segundos) 24))
  
(define (semanas segundos)
  (/ (dias segundos) 7))
  
(define (meses segundos)
  (/ (dias segundos) 30))
  
(define (años segundos)
  (/ (meses segundos) 12))

;(segundos-tiempo 345600 'dias)


;Segundo Ejercicio
(define (ordenado? l)
  (if (null? (cdr l))
    #t
    (if (< (car l) (cadr l))
     (ordenado?(cdr l))
      #f)
    )
  )

;(ordenado? '(1000 2 3 4 5 6 7 8 9))

;Tercer ejercicio
(define (iniciales l)
  (map (lambda (letter)
         (string->symbol
          (string
           (string-ref
            (symbol->string letter) 0))))
       l)
  )
;(iniciales '(Teoría de lenguajes de programación))

;Cuarto Ejercicio
(define (cuenta elem lista cont)
  (cond
   ((null? lista) cont)
   ((equal? elem (car lista)) (cuenta elem (cdr lista) (+ cont 1)))
   (else (cuenta elem (cdr lista) cont))
  )
)

(define (cuenta-elems lista1 lista2 lista3)
(cond
  ((null? lista1) lista3)
  (else (cuenta-elems (cdr lista1) lista2 (cons (list (car lista1) (cuenta (car lista1) lista2 0)) lista3)))))


;(cuenta-elems '(cd 3 2 usb) '(cd cd 3 2 usb 3 2 3) '())


;Quinto ejercicio
(define tree '(40 (28 (9 () ()) (32 () ())) (70 (52 () ()) (102 () ()))))

(define (max-of-tree l Num_max)
(cond
  ((null? l) Num_max)
  ((> (car l)Num_max) (max-of-tree(cdr l) (car l)))
  (else (max-of-tree (cdr l)Num_max))
  )
  )
  
(define (ordena arbol)
(cond
  ((null? arbol) '())
  (else (append
    (ordena (cadr arbol))
    (if (list? (car arbol)) (car arbol) (list (car arbol)))
    (ordena(caddr arbol))))
  )
  )


(max-of-tree (ordena tree) (car(ordena tree)))