#lang racket

#| Ejercicio 1 - sumar-menores |#
(display "Ejercicio 1:\n")
(define (sumaMenores n)
  (cond
    [(= n 0) 0]
    [else (+ n (sumaMenores (- n 1)))])
)

(define (restaS m)
  (sumaMenores (- m 1)))

(restaS 4)

#| Ejercicio 2 - mayor divisor |#
(display "Ejercicio 2:\n")
(define (mayorDivisor n sig)
  (cond
    [(= (modulo n sig) 0) (write sig)]
    [(or(<= n 0) (<= sig 0)) (display "MAL")]
    [else (mayorDivisor n (- sig 1))]))

(define (restaD m)
  (mayorDivisor m (- m 1)))

(restaD 27)

#| Ejercicio 3 - cifra decimal |#
(display "Ejercicio 3:\n")
(define (cifraDecimal decimal numero)
  (cond
  [(= decimal 0) #f]
  [(= (remainder decimal 10) numero) #t]
  [else (cifraDecimal (quotient decimal 10) numero)]))

(cifraDecimal 12345 1)

#| Ejercicio 4 - suma-cifras |#
(display "Ejercicio 4:\n")
(define (sumaCifras decimal)
  (cond
    [(= decimal 0) 0]
    [else (+ (remainder decimal 10) (sumaCifras (quotient decimal 10)))]))

(sumaCifras 1234)





#| Ejercicio 6 - division-euclidea |#
(display "Ejercicio 6:\n")
(define (divisionEu a b loop)
  (cond
    [(or (= a 0) (= b 0)) (list loop a)]
    [(> b a) (list loop a)]
    [else (divisionEu (- a b) b (add1 loop))]
  )
)

(divisionEu 16 3 0)

#| Ejercicio 7 y 8 - potencia |#
(display "Ejercicio 7 y 8:\n")
(define (esPotencia num potencia)
  (cond
    [(= num 1) #t]
    [else (and (equal? (modulo num potencia) 0)
     (esPotencia (quotient num potencia) potencia))]
  )
)

(esPotencia 12 2)

#| Ejercicio 9 - enesima potencia |#
(display "Ejercicio 9:\n")
(define (enesimaPotencia numero n)
  (cond
    [(= n 0) 1]
    [else (* numero (enesimaPotencia numero (- n 1)))]
  )
)

(enesimaPotencia 2 5)

#| Ejercicio 10 - numero igual suma de sus menores |#
(display "Ejercicio 10:\n")
(define (anterior num)
  (sumaAnteriores (- num 1))
)
(define (sumaAnteriores num)
  (cond
    [(<= num 0) 0]
    [else (+ num (sumaAnteriores (- num 1)))]
  )
)
(define (numeroPerfecto numero)
  (if (= (anterior numero) numero) #t #f)
)

(numeroPerfecto 12)

#| Ejercicio 11 - lista numeroPerfecto |#
(display "Ejercicio 11:\n")
(define (anterior2 num)
  (listaAnteriores '() (- num 1))
  )
(define (listaAnteriores lista num)
  (cond
    [(<= num 0)]
    [else (listaAnteriores '(append lista num) (- num 1))])
)

(anterior2 5)

#| Ejercicio 13 - numero igual suma de sus menores divisores |#
(display "Ejercicio 13:\n")
(define (anterior3 num)
  (sumaDivisores (- num 1) num 0)
)
(define (sumaDivisores div num suma)
  (cond
    [(<= div 0) (if (= suma num) #t #f)]
    [(= (remainder num div) 0) (sumaDivisores (- div 1) num (+ suma div))]
    [else (sumaDivisores (- div 1) num suma)]
  )
)
(define (esPerfecto num)
  (if (= (anterior3 num) num) #t #f)
)

(anterior3 496)

#| Ejercicio examen - |#
(display "Ejercicio examen:\n")
(define (pot num potencia loop)
  (cond
    [(= num 1) (write loop)]
    [(< num potencia) #f]
    [else (pot (/ num potencia) potencia (add1 loop))]
  )
)

(pot 16 2 0)

(foldr + 0 (map (lambda (x) (expt x 2)) '(1 2 3 4 5)))
