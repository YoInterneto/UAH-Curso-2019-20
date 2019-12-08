#lang racket

#| Ejercicio 1 - sumar-menores |#
(display "Ejercicio 1:\n")
(define (sumar-menores num)
  (if (= num 0)
      0
      (+ num (sumar-menores(- num 1)))
   )
)
(sumar-menores 4)

#| Ejercicio 2 - buscar-divisor-mayor |#
(display "Ejercicio 2:\n")
(define (buscar-divisor-mayor num)
  (define (divisor num div)
    (cond
      [(= 0 (modulo num div)) div]
      [else (divisor num (- div 1))]
      )
  )
  (divisor num (- num 1))
)
(buscar-divisor-mayor 8)

#| Ejercicio 3 - contiene-cifra |#
(display "Ejercicio 3:\n")
(define (contiene-cifra numero cifra)
  (cond
    [(equal? numero 0) #f]
    [(equal? (remainder numero 10) cifra) #t]
    [else (contiene-cifra (quotient numero 10) cifra)]
  )
)
(contiene-cifra 1234567 3)

#| Ejercicio 4 - suma-cifras |#
(display "Ejercicio 4:\n")
(define (suma-cifras numero)
  (cond
    [(equal? numero 0) 0]
    [else (+ (remainder numero 10) (suma-cifras (quotient numero 10)))]
  )
)
(suma-cifras 1234)

#| Ejercicio 6 - division-euclidea |#
(display "Ejercicio 6:\n")
(define (division-euclidea numerador denominador)
  (define (div num denom acc)
     (cond
       [(< num denom) (list acc num)]
       [else (div (- num denom) denom (add1 acc))]
     )
   )
   (div numerador denominador 0)
)
(division-euclidea 10 3)

#| Ejercicio 8 - potencia? |#
(display "Ejercicio 8:\n")
(define (potencia? numero potencia)
  (if (<= numero 1)
      #t
      (and (equal? (modulo numero potencia) 0)
           (potencia? (quotient numero potencia) potencia)
      )
  )
)
(potencia? 6 2)