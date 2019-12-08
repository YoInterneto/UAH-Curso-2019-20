#lang racket

(define (factorialX x)
  (cond
       [(= x 0) 1]
       [(> x 0) (* x (factorialX(- x 1)))]
       [else (display "ERROR: El factorial no puede ser menor a 0\n")]))

(factorialX 5)
(factorialX 0)
(factorialX -1)

(define (sumaLista lista)
  (cond
    [(empty? lista) 0]
    [else (+ (car lista) (sumaLista(cdr lista)))]
  )
)

(define (sumaLista2 lista)
  (if(empty? lista) 0 (+ (car lista) (sumaLista(cdr lista)))))

(define (mediaLista . lista)
  (cond
    [(empty? lista) 0]
    [else (/ (apply + lista) (length lista))]))

(define lista (list 5 5 5 5 3 3 3 3 ))
(mediaLista lista)
(mediaLista 0 1)