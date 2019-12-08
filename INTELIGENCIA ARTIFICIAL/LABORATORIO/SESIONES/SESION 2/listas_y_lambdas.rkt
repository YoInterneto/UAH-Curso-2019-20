#lang racket

;; Equivalente a función map de Racket
;; Procesa una lista aplicando una función a cada uno de los elementos
;; y devuelve una nueva lista con los elementos procesados
(define (mapear lista f)
  (if (empty? lista) empty
      (cons (f (car lista)) (mapear (cdr lista) f))))

;; Equivalente a función filter de Racket
;; Procesa una lista aplicando un predicado a cada uno de los elementos
;; y devuelve una nueva lista filtrara con los elementos que satisfacen el predicado
(define (filtrar lista f)
  (if (empty? lista) empty
      (if (f (car lista))
          (cons (car lista) (filtrar (cdr lista) f))
          (filtrar (cdr lista) f))))

;; Ejemplo de predicado para usar con filter
(define (mayorque5 a)
  (> a 5))

;; Ejemplo de uso de función foldr
;; Procesa una lista aplicando una función a cada uno de los elementos
;; y a un acumulador, devolviendo al final el valor del acumulador 
(define (suma-elementos lista)
  (foldr (lambda (x accum) (+ x accum)) 0 lista))

;; Ejemplo de uso de función foldl
;; Igual que foldr, pero procesa la lista en orden inverso
(define (invertir lista)
  (foldl cons empty lista))
