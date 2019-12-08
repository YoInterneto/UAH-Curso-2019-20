#lang racket

(define matriz '((5 0 0  0 0 0  0 0 0)
                 (0 2 8  4 0 0  5 0 3)
                 (1 0 0  2 7 0  0 0 6)
                 
                 (0 0 3  0 5 2  1 9 0)
                 (7 0 6  0 1 0  2 0 8)
                 (0 1 9  7 4 0  3 0 0)
                 
                 (6 0 0  0 9 4  0 0 2)
                 (8 0 1  0 0 6  7 5 0)
                 (0 0 0  0 0 0  0 0 4)))


#|=====================================================================
   Func: estaFila : list number number number -> boolean
   Obj: Devuelve #t cuando el numero esta en la fila y
        #f cuando no lo esta
  =====================================================================|#
(define (estaFila matriz fila columna numero)
  (for/or ([c 9])
    (if (= c columna) ;si es la misma posicion no la compara
        #f
        (equal? (list-ref (list-ref matriz fila) c) numero) ;compara la posicion para las demas posiciones
    )
  )
)


#|=====================================================================
   Func: estaColumna : list number number number -> boolean
   Obj: Devuelve #t cuando el numero esta en la columna y
        #f cuando no lo esta
  =====================================================================|#
(define (estaColumna matriz columna fila numero)
  (for/or ([f 9])
    (if (= f fila) ; si es la misma posicion no la compara
        #f
        (equal? (list-ref (list-ref matriz f) columna) numero) ;compara la posicion para las demas posiciones
    )
  )
)


#|===========================================================================
   Func: estaCuadrado list number number number number number -> boolean
   Obj: Devuelve #t cuando el numero esta en el cuadrado y
        #f cuando no lo esta
  ===========================================================================|#
(define (estaCuadrado matriz inicio final fila columna numero)
  (for*/or ([f (in-range inicio (+ 3 inicio))][c (in-range final (+ 3 final))])
    (if (and (= f fila) (= c columna)) ; si es la misma posicion no la compara
        #f
        (equal? (list-ref (list-ref matriz f) c) numero) ;compara la posicion para las demas posiciones
    ) 
  )
)


#|===============================================================
   Func: estaPermitido : list number number number -> boolean
   Obj: Devulve #t si el numero puede ponerse en esa posicion
        y #f si no puede
  ===============================================================|#
(define (estaPermitido matriz fila columna numero)
  (if (or (estaCuadrado matriz (- fila (remainder fila 3)) (- columna (remainder columna 3)) fila columna numero) ;aqui iria el que ve si esta en el mismo cuadro
          (estaFila matriz fila columna numero)
          (estaColumna matriz columna fila numero)
      )
      #f #t)
)


#|===============================================================
   Func: sudokuCorrecto? : list -> boolean
   Obj: Devulve #t si el sudoku es correcto y #f si no lo es
  ===============================================================|#
(define (sudokuCorrecto? sudoku)
  (for*/and ([f 9][c 9])
    (cond
      [(not (equal? (list-ref (list-ref sudoku f) c) 0))
       (estaPermitido sudoku f c (list-ref (list-ref sudoku f) c))]
      [else #t]
    )
  )
)


#|===============================================================
   Func: buscarNum : list number number -> boolean
   Obj: Comprueba que numeros son validos en cada posicion
  ===============================================================|#
(define (buscarNum matriz fila columna)
  (for ([f 9])
    (for ([c 9])
      (if (=(list-ref(list-ref matriz f)c) 0)
          (for ([i (in-range 1 10)])
            (if (estaPermitido matriz f c i) 
               (display (insert-at c i (list-ref matriz f)))
            (display "\n")
            ))
      (printf "Esta el numero ~a en f:~a c:~a \n" (list-ref(list-ref matriz f)c) f c)
      ))))
          
#|=============================================================
   Func: insert-at : number number list -> list
   Obj: Delvuelve la lista con el elemento insertado en la
        posicion indicada
  ===============================================================|#
(define (insert-at pos elmt lst)
 (if (empty? lst) (list elmt)
 (if (= 0 pos)
  (cons elmt lst)
  (cons (first lst) 
        (insert-at (- pos 1) elmt (rest lst))))))

;(take (insert-at 1 3 (list-ref matriz 0))2)
(buscarNum matriz 1 0)
(display matriz)
(sudokuCorrecto? matriz)
