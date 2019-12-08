#lang racket


(define matriz '((0 1 0  0 2 1  0 5 6)
                 (0 0 0  1 7 9  8 4 5)
                 (2 6 8  9 5 0  0 1 5)
                 
                 (4 7 5  2 0 3  6 5 4)
                 (0 0 0  2 4 5  8 7 4)
                 (2 0 5  8 4 2  0 0 1)
                 
                 (0 2 5  0 0 3  6 0 0)
                 (0 5 0  0 2 9  8 7 3)
                 (0 0 2  0 0 5  8 2 1)))


(define matriz1 '((3 0 0  0 0 0  0 0 0)
                  (0 2 8  4 0 0  5 0 3)
                  (1 0 5  2 7 0  0 0 6)

                  (0 0 3  0 5 2  1 9 0)
                  (7 0 6  0 1 0  2 0 8)
                  (0 1 9  7 4 0  3 0 0)

                  (5 0 0  0 9 3  0 0 2)
                  (8 0 1  0 0 6  7 5 0)
                  (0 0 0  0 0 0  0 0 4)))


#|=====================================================================
   Func: estaFila : list number number -> boolean
   Obj: Devuelve #t cuando el numero esta en la fila y
        #f cuando no lo esta
  =====================================================================|#
(define (estaFila matriz fila numero)
  (for/or ([c 9])
    (equal? (list-ref (list-ref matriz fila) c) numero)
  )
)


#|=====================================================================
   Func: estaColumna : list number number -> boolean
   Obj: Devuelve #t cuando el numero esta en la columna y
        #f cuando no lo esta
  =====================================================================|#
(define (estaColumna matriz columna numero)
  (for/or ([f 9])
    (equal? (list-ref (list-ref matriz f) columna) numero)
  )
)


#|===========================================================================
   Func: estaCuadrado list number number number -> boolean
   Obj: Devuelve #t cuando el numero esta en el cuadrado y
        #f cuando no lo esta
  ===========================================================================|#
(define (estaCuadrado matriz fila columna numero)
  (for*/or ([f (in-range fila (+ 3 fila))][c (in-range columna (+ 3 columna))])
    (equal? (list-ref (list-ref matriz f) c) numero)
  )
)


#|===============================================================
   Func: estaPermitido : list number number number -> boolean
   Obj: Devulve #t si el numero puede ponerse en esa posicion
        y #f si no puede
  ===============================================================|#
(define (estaPermitido matriz fila columna numero)
  (if (or (estaCuadrado matriz (- fila (remainder fila 3)) (- columna (remainder columna 3)) numero) ;aqui iria el que ve si esta en el mismo cuadro
          (estaFila matriz fila numero)
          (estaColumna matriz columna numero)
      )
      #f #t)
)

(estaPermitido matriz1 2 1 5)
(newline)
(estaPermitido matriz1 3 4 9)
(newline)
(estaPermitido matriz1 8 0 2)
(newline)
(estaPermitido matriz1 8 8 3)
(newline)
(estaPermitido matriz1 6 6 4)
(newline)
(estaPermitido matriz1 5 6 7)
(newline)
(estaPermitido matriz1 0 0 2)
(newline)
(estaPermitido matriz1 1 8 3)
(newline)
(estaPermitido matriz1 7 3 4)
(newline)
(estaPermitido matriz1 0 6 9)