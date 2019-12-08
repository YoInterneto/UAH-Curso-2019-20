#lang racket

;TIPOS
(define-struct arbol (raiz izq der))

;FUNCIONES
#|==========================================================
   Func: arbol-vacio : arbol -> bool
   Obj: Devuelve si el arbol es vacío
  ==========================================================|#
(define (arbol-vacio? arbol)
  (empty? arbol)
)

#|==========================================================
   Func: insertar : dato arbol -> arbol
   Obj: Inserta el dato en el arbol
  ==========================================================|#
(define (insertar elem arbol)
  (if (arbol-vacio? arbol)
      (make-arbol elem empty empty)
      (let ([raiz (arbol-raiz arbol)])
        (cond
          [(> raiz elem) (make-arbol raiz
                                     (insertar elem (arbol-izq arbol))
                                     (arbol-der arbol)
                         )
          ]
          [(< raiz elem) (make-arbol raiz
                                     (arbol-izq arbol)
                                     (insertar elem (arbol-der arbol))
                         )
          ]
          [else arbol]
        )
      )
  )
)

#|==========================================================
   Func: pertenece : dato arbol -> bool
   Obj: Comprueba si un dato está en el arbol
  ==========================================================|#
(define (pertenece? elem arbol)
  (if (arbol-vacio? arbol)
      #f
      (let ([raiz (arbol-raiz arbol)])
        (cond
          [(> raiz elem) (pertenece? elem (arbol-izq arbol))]
          [(< raiz elem) (pertenece? elem (arbol-der arbol))]
          [else #t]
        )
      )
  )
)

#|==========================================================
   Func: arbol-to-list : arbol -> list
   Obj: Transforma el arbol en una lista estructurada
  ==========================================================|#
(define (arbol-to-list arbol)
  (cond
    [(arbol-vacio? arbol) empty]
    [else (append (list (arbol-raiz arbol))
                  (list (arbol-to-list (arbol-izq arbol)))
                  (list (arbol-to-list (arbol-der arbol)))
          )
    ]
  )
)

#|==========================================================
   Func: list-to-arbol : list -> arbol
   Obj: Transforma una lista estructurada en un arbol
  ==========================================================|#
(define (list-to-arbol lista)
  (cond
    [(empty? lista) '()]
    [else (make-arbol (car lista)
                      (list-to-arbol (cadr lista))
                      (list-to-arbol (caddr lista))
          )
    ]
  )
)

#|==========================================================
   Func: preorden : arbol -> list
   Obj: Devuelve una lista con el recorrido en preorden
  ==========================================================|#
(define (preorden arbol)
  (cond
    [(arbol-vacio? arbol) empty]
    [else (append (list (arbol-raiz arbol))
                  (preorden (arbol-izq arbol))
                  (preorden (arbol-der arbol))
          )
    ]
  )
)
#|==========================================================
   Func: postorden : arbol -> list
   Obj: Devuelve una lista con el recorrido en postorden
  ==========================================================|#
(define (postorden arbol)
  (cond
    [(arbol-vacio? arbol) empty]
    [else (append (postorden (arbol-izq arbol))
                  (postorden (arbol-der arbol))
                  (list (arbol-raiz arbol))
          )
    ]
  )
)

#|==========================================================
   Func: en-orden : arbol -> list
   Obj: Devuelve una lista con el recorrido en orden
  ==========================================================|#
(define (en-orden arbol)
  (cond
    [(arbol-vacio? arbol) empty]
    [else (append (en-orden (arbol-izq arbol))
                  (list (arbol-raiz arbol))
                  (en-orden (arbol-der arbol))
          )
    ]
  )
)

;PROGRAMA PRINCIPAL
(define miarbol (make-arbol 10 '() '()))
(display "Arbol B-Tree con un 10:\n")
(arbol-to-list miarbol)
(display "Inserto el 9:\n")
(set! miarbol (insertar 9 miarbol))
(arbol-to-list miarbol)
(display "Inserto el 19:\n")
(set! miarbol (insertar 19 miarbol))
(arbol-to-list miarbol)
(display "Creo el mismo arbol con una lista:\n")
(set! miarbol (list-to-arbol '(10 (9 () ()) (19 () ()))))
(arbol-to-list miarbol)
(display "Inserto el 12:\n")
(set! miarbol (insertar 12 miarbol))
(arbol-to-list miarbol)
(display "Inserto el 1:\n")
(set! miarbol (insertar 1 miarbol))
(arbol-to-list miarbol)
(display "Ejemplos de uso del predicado pertenece?\n")
(display "(pertenece? 1) --> ")
(pertenece? 1 miarbol)
(display "(pertenece? 13) --> ")
(pertenece? 13 miarbol)
(display "(pertenece? 5) --> ")
(pertenece? 5 miarbol)
(display "Recorrido en preorden:\n")
(preorden miarbol)
(display "Recorrido en postorden:\n")
(postorden miarbol)
(display "Recorrido en orden:\n")
(en-orden miarbol)




(- 4 (remainder 4 3))