#lang racket
;Realizado por Rafael Perea

;Primer ejercicio
(define (segundos-tiempo numero tipo)
  (cond  
      [(equal? tipo 's) (string-append (~a numero)  " segs")]
      [(equal? tipo 'm) (string-append (~a (minutos numero)) " min")]
      [(equal? tipo 'h) (string-append (~a (horas numero)) " horas")]
      [(equal? tipo 'd) (string-append (~a (dias numero)) " dias")]
      [(equal? tipo 'sem) (string-append (~a (semanas numero)) " semanas")]
      [(equal? tipo 'mes) (string-append (~a (meses numero)) " mes/meses")]
      [(equal? tipo 'annio) (string-append (~a (annios numero)) " años")]
  )
)

(define (minutos segundos)
  (/ segundos 60))

(define (horas segundos)
  (/ (minutos segundos) 60))

(define (dias segundos)
  (/ (horas segundos) 24))
  
(define (semanas segundos)
  (/ (dias segundos) 7))
  
(define (meses segundos)
  (/ (dias segundos) 30))
  
(define (annios segundos)
  (/ (meses segundos) 4))

(segundos-tiempo 2592000 'mes)


;Segundo Ejercicio
(define (ordenado? l)
  (if (null? (cdr l))
    #t
    (if (< (car l) (cadr l))
     (ordenado?(cdr l))
      #f)
    )
  )

;(ordenado? '(1 2 4 8 9 300))

;Tercer ejercicio
(define (iniciales l)
  (map (lambda (letter)
         (string->symbol
          (string
           (string-ref
            (symbol->string letter) 0))))
       l)
  )
;(iniciales '(Lenguajes y Paradigmas de Programación))

;Cuarto Ejercicio
(define (cuenta elem lista cont)
  (cond
   ((null? lista) cont)
   ((equal? elem (car lista)) (cuenta elem (cdr lista) (+ cont 1)))
   (else (cuenta elem (cdr lista) cont))
  )
)

(define (pares l1 l2 l3)
(cond
  ((null? l1) l3)
  (else (pares (cdr l1) l2 (cons (list (car l1) (cuenta (car l1) l2 0)) l3)))))

;(pares '(a b c e f) '(a b b b c d d a e e f a a) '())
;(pares '(hola 3 2) '(hola hola 3 2 hola 3 2 3) '()) 