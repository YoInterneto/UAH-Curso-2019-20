#lang racket

;EJERCICIO 5
(write "EJERCICIO 5") (newline)
(define (anadirElem a lista)
  ((append lista a)))

(append (list 1 2 3 4) (list 69))

;EJERCICIO 6
(write "EJERCICIO 6") (newline)
(define (printear n) n )
(define (encontrar lista)
  (write (second lista))
  (newline)
  (write lista)
  (newline)
  (write(reverse lista))
  (newline)
  (write (second(reverse lista))
))

(define lista (list 1 2 3 4))
(encontrar lista)

;EJERCICIO 7
(write "EJERCICIO 7") (newline)
(define (dosElem lista)
  (cond
    [(>= (length lista) 2) (write "AL MENOS DOS")]
    [else (write "MENOS DE DOS :(")]
   )
  (newline)
  )

(dosElem lista)

;EJERCICO 8
(write "EJERCICIO 8") (newline)
(define (longitud lista n)
  (cond
    [(empty? lista) (display "Longitud-> ")(write n) (newline)]
    [else (longitud (cdr lista) (+ n 1))]))

(define (longitud2 n lista)
  (for ([valor lista]) (+ n 1))
)

(longitud lista 0)
(longitud2 lista 0)

;EJERCICIO 9
(write "EJERCICIO 9") (newline)
(define (reves lista lista1 index)
  (cond
    [(< index 0) (write "Lista del reves-> ")(write lista1) (newline)]
    [else (reves lista (append lista1 (list(list-ref lista index))) (- index 1))]
  )
)

(foldl cons null lista)
(reves lista '() (- (length lista) 1))

;EJERCICIO 10
(write "EJERCICIO 10") (newline)
;(apply + lista)
(define (sumaLista lista suma)
  (cond
    [(empty? lista) (write "Suma de lista -> ")(write suma) (newline)]
    [else (sumaLista (cdr lista) (+ suma (car lista)))])
  )

(sumaLista lista 0)

;EJERCICIO 11
(write "EJERCICIO 11") (newline)
(define (cuadradosSuma lista suma)
  (cond
    [(empty? lista) (display "Suma de cuadrados -> ")(write suma) (newline)]
    [else (cuadradosSuma (cdr lista) (+ suma (expt (car lista) 2)))])
  )

(cuadradosSuma lista 0)

;EJERCICIO 12
(write "EJERCICIO 12") (newline)
(define (progresionA lista razon anterior longitud )
  (cond
    [(empty? lista)  #t]
    [(= longitud (length lista)) (progresionA (cdr lista) razon (car lista) longitud)] ;Primera iteraxcion
    [(= (- longitud 1) (length lista)) (progresionA (cdr lista) (- (car lista) anterior) (car lista) longitud)] ;Segunda iteracion
    [else (cond ;Demas iteraciones
            [(not(= razon (- (car lista) anterior))) #f] ;Cuando no es igual
            [else (progresionA (cdr lista)  (- (car lista) anterior) (car lista) longitud)])] ;Es igual y quedan elementos
  )
)

(define (progresionG lista razon anterior longitud)
  (cond
    [(empty? lista)  #t]
    [(= longitud (length lista)) (progresionG (cdr lista) razon (car lista) longitud)] ;Primera iteraxcion
    [(= (- longitud 1) (length lista)) (progresionG (cdr lista) (/ (car lista) anterior) (car lista) longitud)] ;Segunda iteracion
    [else (cond ;Demas iteraciones
            [(not(= razon (/ (car lista) anterior))) #f] ;Cuando no es igual
            [else (progresionG (cdr lista)  (/ (car lista) anterior) (car lista) longitud)])] ;Es igual y quedan elementos
    )
)

(define (progresion lista)
  (cond
    [(and (progresionA lista 0 0 (length lista)) (progresionG lista 0 0 (length lista))) (write "Es geometrica y aritmetica")]
    [(and (progresionA lista 0 0 (length lista)) #t) (write "Es solo aritmetica")]
    [(and (progresionG lista 0 0 (length lista)) #t) (write "Es solo geometrica")]
    [else (write "NO es geometrica ni aritmetica")]
  )
)

(define lista1 (list 0))
(define lista2 (list 2 4))
(define lista3 (list 2 4 8 16))
(define lista4 (list 1 2 3 4 5 6 7 8 9 10))
(define lista5 (list 1 2 6 4 5 90 7 8 9 10))

(write lista1)(display "-> ")(progresion lista1)(newline)
(write lista2)(display "-> ")(progresion lista2)(newline)
(write lista3)(display "-> ")(progresion lista3)(newline)
(write lista4)(display "-> ")(progresion lista4)(newline)
(write lista5)(display "-> ")(progresion lista5)(newline)

;EJERCICIO 13
(write "EJERCICIO 13") (newline)
(define (repartir mayor menor cuantos)
  (for ([value menor])
    (for ([n cuantos])
      (printf "~a con ~a - " value (list-ref mayor n))
    )
    (newline)
  )
)

(define (masMenos monitores alumnos)
  (define x (length monitores))
  (define y (length alumnos))
  (cond
    [(or (= x 0) (= y 0)) (write "No hay alumnos/monitores") (newline)]
    [(> (length monitores)(length alumnos)) (repartir monitores alumnos (/ x y))]
    [else (repartir alumnos monitores (/ y x))]
    )
  )
(define monitores(list "M1" "M2" "M3"))
(define alumnos(list "A1" "A2" "A3" "A4" "A5" "A6"))
(masMenos monitores alumnos) 
    

;EJERCICIO 15
(write "EJERCICIO 15") (newline)
(define (cadenaReves lista vacia)
  (foldl cons vacia lista)
  )

(define palabraa '(1 2 3 4))
(cadenaReves palabraa null)

;EJERCICIO 16
(write "EJERCICIO 16") (newline)
(define palabra '(n a r r a n))
(define palabra1 '(h o l a t i o))
(write palabra)(display "-> ")
(for/and ([letra1 palabra][letra2 (reverse palabra)]) (equal? letra1 letra2))
(write palabra1)(display "-> ")
(for/and ([letra1 palabra1][letra2 (reverse palabra1)]) (equal? letra1 letra2))