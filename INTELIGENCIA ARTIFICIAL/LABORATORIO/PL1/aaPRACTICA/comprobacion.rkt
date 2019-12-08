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

(display matriz)
(newline)

;(remainder 3 4)
;(for ([i (in-range 1 10)]) (display i))
;(display (list-ref (list-ref matriz fila) columna))


#|=====================================================================
   Func: estaFila : list number number number number -> boolean
   Obj: Devuelve #t cuando el numero esta en la fila y
        #f cuando no lo esta
  =====================================================================|#
(define (estaFila? matriz fila loop numero)
  (cond
    [(= loop 9) ;si llega al final y no encuentra el valor igual sera false
     #f]
    
    [(= (list-ref (list-ref matriz fila) loop) numero) ;si lo encuentra sera true
     #t (printf "se encuentra ~a en (f:~a c:~a)" numero fila loop)]
    
    [else
     (estaFila? matriz fila (add1 loop) numero)]
    )
)




#|=====================================================================
   Func: estaColumna : list number number number number -> boolean
   Obj: Devuelve #t cuando el numero esta en la columna y
        #f cuando no lo esta
  =====================================================================|#
(define (estaColumna? matriz columna loop numero)
  (cond
    [(= loop 9) ;si llega al final y no encuentra el valor igual sera false
     #f]
    
    [(= (list-ref (list-ref matriz loop) columna) numero) ;si lo encuentra sera true
     #t (printf "se encuentra ~a en (f:~a c:~a)" numero loop columna)]
    
    [else
     (estaColumna? matriz columna (add1 loop) numero)]
    )
)


#|===========================================================================
   Func: estaCuadrado list number number number number number -> boolean
   Obj: Devuelve #t cuando el numero esta en el cuadrado y
        #f cuando no lo esta
  ===========================================================================|#
(define (estaCuadrado? matriz fila columna numero loopF loopC)
  (cond
    [(= (list-ref (list-ref matriz (+ fila loopF)) (+ columna loopC)) numero) ; si lo encuentra entonces true
     #t (printf "se encuentra ~a en su cuadrado (f:~a c:~a)" numero (+ fila loopF) (+ columna loopC))]
    
    [(and (= loopF 2) (= loopC 2)) ;si ha comprobado todos los numeros y no lo encuentra entonces y la ultima esquina no es el numero que se busca entonces false
     #f]

    [(= loopF 2) ; si loopF ha llegado a dos, quiere decir que se ha comprobado toda la primera columna
                 ; con lo que empezamos a ver la siguiente columna
     (estaCuadrado? matriz fila columna numero (- loopF 2) (add1 loopC))]

    [else ;si loopF no es igual a dos seguimos iterando la columna hasta llegar al final de esta
     (estaCuadrado? matriz fila columna numero (add1 loopF) loopC)]
    
    )
)



#|===============================================================
   Func: estaPermitido : list number number number -> boolean
   Obj: Devulve #t si el numero puede ponerse en esa posicion
        y #f si no puede
  ===============================================================|#
(define (estaPermitido? matriz fila columna numero)
  (if (or (estaCuadrado? matriz (- fila (remainder fila 3)) (- columna (remainder columna 3)) numero 0 0) ;aqui iria el que ve si esta en el mismo cuadro
          (estaFila? matriz fila 0 numero)
          (estaColumna? matriz columna 0 numero)
      )
      #f #t)
)

(define (estaPermitidoPrint? matriz fila columna numero)
  (if (estaPermitido? matriz1 fila columna numero)
      (printf "esta permitido poner el numero ~a en (f:~a c:~a)" numero fila columna)
      (printf " -> no esta permitido")))



(define (sudokuCorrecto? sudoku)
  (for*/and ([f 9][c 9])
    (cond
      [(not (equal? (list-ref (list-ref sudoku f) c) 0))
       (estaPermitido? sudoku f c (list-ref (list-ref sudoku f) c))]
      [else #t]
    )
  )
)


(sudokuCorrecto? matriz)
(sudokuCorrecto? matriz1)


#|(estaPermitidoPrint? matriz1 2 1 5)
(newline)
(estaPermitidoPrint? matriz1 3 4 9)
(newline)
(estaPermitidoPrint? matriz1 8 0 2)
(newline)
(estaPermitidoPrint? matriz1 8 8 3)
(newline)
(estaPermitidoPrint? matriz1 6 6 4)
(newline)
(estaPermitidoPrint? matriz1 5 6 7)
(newline)
(estaPermitidoPrint? matriz1 0 0 2)
(newline)
(estaPermitidoPrint? matriz1 1 8 3)
(newline)
(estaPermitidoPrint? matriz1 7 3 4)
(newline)
(estaPermitidoPrint? matriz1 0 6 9)|#
