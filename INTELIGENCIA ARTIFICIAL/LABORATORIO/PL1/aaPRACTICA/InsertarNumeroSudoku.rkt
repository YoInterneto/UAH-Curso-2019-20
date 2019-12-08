#lang racket
(define matriz '((0 0 0  0 2 1  0 5 6)
                 (0 0 0  1 7 9  8 4 5)
                 (2 6 8  9 5 0  0 1 5)
                 
                 (4 7 5  2 0 3  6 5 4)
                 (0 0 0  2 4 5  8 7 4)
                 (2 0 5  8 4 2  0 0 1)
                 
                 (0 2 5  0 0 3  6 0 0)
                 (0 5 0  0 2 9  8 7 3)
                 (0 0 2  0 0 5  8 2 1)))

#|===============================================================
   Func: insertar : list list number number number -> list
   Obj: Devuelve la lista del sudoku después de insertar el nodo
  ===============================================================|#
(define (insertar lista lista2 fila columna numero contador)
  (cond [(= contador 9) lista2]
        ;si esta en la posicion que se quiere cambiar se cambia
        [(= fila contador) (insertar lista (append lista2 (list (introducirNumero (list-ref lista contador) '() columna numero))) fila columna numero (+ contador 1))]
        ;si no esta en la posicion que se quiere cambiar no hace nada
        [else (insertar lista (append lista2 (list (list-ref lista contador))) fila columna numero (+ contador 1))]))
        
    



#|===============================================================
   Func: crearNodo : list list number number -> list
   Obj: Devuelve una lista con el elemento cambiado en la posicion indicada
  ===============================================================|#
(define (introducirNumero lista lista2 posicion numero)
    (cond [(= (length lista) 0) lista2]
          [(= posicion 0) (introducirNumero (cdr lista) (append lista2 (list numero)) (- posicion 1) numero)]
          [else (introducirNumero (cdr lista) (append lista2 (list (car lista))) (- posicion 1) numero)]))


(insertar matriz '() 3 3 5 0)
