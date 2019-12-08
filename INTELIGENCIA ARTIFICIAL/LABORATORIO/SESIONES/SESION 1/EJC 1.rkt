#lang racket
;EJERCICIO 1
(define (calculadora a b c)
  (cond
    [(equal? a "suma") (+ b c)]
    [(equal? a "resta") (- b c)]
    [(equal? a "division") (/ b c)]
    [(equal? a "multiplicacion") (* b c)]
    [else "Operador no reconocido"]))

(calculadora "suma" 3 5)
(calculadora "resta" 3 5)
(calculadora "division" 3 5)
(calculadora "multiplicacion" 3 5)
(calculadora "hola" 3 5)

;EJERCICIO 2 Y 3
(define pi 3.1415)
(define e 2.7182)
(define i 1)

(expt 3 1/2) ;raiz cuadrada 3
(expt 3 2/2) ;raiz cuadrada 3 ^ 2
(expt e (* i pi 74))
(log 3 10)
(log 2 10)

;EJERCICIO 4
(define (angulo a)
  (* (acos a) 57.2958))

(angulo 0.846)

;EJERCICIO 5
(define (numeroE numero)
  (exp numero))
(numeroE 1)

;EJERCICIO 6
(define x #t)
(define y #f)
(define z #t)

(or y (and (not y) z #t)) ; (or #f (and #t #t #t)) -> (#t) si z es true
                         ; (or #f (and #t #t #f)) -> (#f) si z es false

;EJERCICIO 8
(define (nota a)
  (cond
    [(and (>= a 0)(< a 5)) "SUSPENSO MAQUINA"]
    [(and (>= a 5) (< a 6)) "RASPAO"]
    [(and (>= a 6) (< a 7)) "BIEN"]
    [(and (>= a 7) (< a 9)) "NOTABLE"]
    [(and (>= a 9) (< a 10)) "SOBRESALIENTE"]
    [else "NO SE QUE QUIERES"]))

(nota -1)
(nota 0)
(nota 5.5)
(nota 6.9)
(nota 8)
(nota 10)
(nota 12)

;EJERCICIO 9
(define (dado)
  ( + 1 (random 6)))
(dado)

;EJERCICIO 10
(define (2grado+ a b c)
  (writeln "SOLUCIONES:")
  (/ (+ (* (- b (* b 2))) (expt (- (expt b 2)(* 4 a c))1/2))(* 2 a)))

(define (2grado- a b c)
  (writeln "SOLUCIONES:")
  (/ (- (* (- b (* b 2))) (expt (- (expt b 2)(* 4 a c))1/2))(* 2 a)))

(2grado+ 1 -2 -35)
(2grado- 1 -2 -35)

;EJERCICIO 11
(define (positivo a)
  (cond
    [(positive? a) "POSITIVO"]
    [else "ERES TONTO"]))

(positivo 5)
(positivo 0)
(positivo -1)

;EJERCICIO 12
(define (valorPositivo a)
  (cond
    [(and (positive? a) (number? a)) "ES POSITIVO"]
    [(number? a) "NO ES POSITIVO"]
    [else "NO ES NUMERO"]))

(valorPositivo -1)
(valorPositivo 5)
(valorPositivo 0)

;EJERCICIO 14
(define (polinomio c x)
  (cond
    [(= c 0) 0]
    [else (+ (* c (expt x (- c 1))) (polinomio (- c 1) x))]))

(polinomio 4 4)


;EJERCICIO PAG
(define (sumaN suma numero)
  (cond
    [(< numero 3) 0]
    [else (cond
            [(= (remainder numero 3) 0) (sumaN (- numero 1) (+ suma numero))]
            [else (sumaN (- numero 1) suma)])]))


(sumaN 25 0)

;EJERCICIO RE 1
(define (factorial numero solucion)
  (cond
    [(<= numero 1) 1]
    [else (factorial (- numero 1) (+ solucion numero))]))

(print(factorial 0 0))
(print(factorial 5 0))
(factorial 5 0)