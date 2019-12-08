#lang racket

#| Ejercicio 7 - adivina-numero |#
(display "Ejercicio 7:\n")
(define (adivina-numero)
  (begin
    (display "Intenta adivinar el número aleatorio entre 0 y 100.\n")
    (define numero (random 100))
    (define num-preguntas 5)
    (define (adivina numero num-preguntas)
      (display "Introduce un número: ")
      (define mi-numero (string->number (read-line (current-input-port) 'any)))
      (cond
        [(equal? numero mi-numero) (display "Has acertado.\n")]
        [(equal? num-preguntas 1) (display (string-append "No has acertado, el número es: " (number->string numero) "\n"))]
        [else (cond
                [(< mi-numero numero) (display "El número introducido es menor.\n") (adivina numero (sub1 num-preguntas))]
                [(> mi-numero numero) (display "El número introducido es mayor.\n") (adivina numero (sub1 num-preguntas))]
              )
        ]
      )
    )
    (adivina numero num-preguntas)
  )
)
(adivina-numero)

