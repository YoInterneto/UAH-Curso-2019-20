#lang racket

(define lista '(1 2 3 4 5 6 7 8 9 10))
(define lista2 '(a d r i a n))

; ejemplos varios de map, filter, foldr/foldl y lambdas
(define (mult10 x) (* 10 x))
(map mult10 lista)
(map (lambda (x) (* 10 x)) lista)

(filter odd? lista) 
(filter (lambda (x) (= (modulo x 2) 1)) lista)

(foldr + 0 lista)
(foldr (lambda (x y) (+ x y)) 0 lista)

(foldr cons empty lista)
(foldl cons empty lista)
(foldl (lambda (x list) (if (odd? x) (cons x list) list)) empty lista)
(foldr (lambda (x counter) (add1 counter)) 0 lista)

; for con variable i que va de 0 a 10
(for ([i 10]) (printf "~a" i))

; for permite recorrer 1 o varias listas simultaneamente en paralelo
; la primera parte del for me permite declarar variables para recorrer las listas
; la segunda parte contiene el cuerpo del for que se aplicará sobre los elementos de las listas
(for
    ([value lista2]                     ; value va a ir tomando el valor de cada elemento de lista2
     [index (in-range (length lista2))] ; (in-range) genera un rango de valores para index
     [index2 (length lista2)])          ; aunque se puede usar de forma implícita, como en index2
  (printf "~a ~a ~a\n" value index index2)
  )

; for tiene muchas variantes, for/and y for/or combinan el resultado de todas las iteraciones mediante and u or
; en este caso comprueba si alguno de los elementos de lista es impar
(printf "¿Hay alguno impar?\n")
(for/or ([value lista]) (odd? value)) ; El cuerpo del for debe devolver true o false para poder combinarlo
; en este caso comprueba si todos los elementos de la lista son impares
(printf "¿Son todos impares?\n")
(for/and ([value '(1 3 5 7 9)]) (odd? value))

; todos los for tienen una variante for* que recorre las listas de manera anidada
(printf "Recorrido listas paralelo\n")
(for ([l1 lista] [l2 lista2]) (printf "~a ~a\n" l1 l2)) ; recorrido en paralelo
(printf "Recorrido listas anidadas\n")
(for* ([l1 lista] [l2 lista2]) (printf "~a ~a\n" l1 l2)) ; recorrido anidado