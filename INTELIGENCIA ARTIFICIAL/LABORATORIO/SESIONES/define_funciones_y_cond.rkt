#lang racket

;; NOTAS SOBRE DEFINE Y FUNCIONES EN RACKET

;; Define se puede utilizar para asignar variables
;; y funciones, lo que se diferencia por los paréntesis
;; alrededor del identificador
(define num5 5) ;; Variable con el valor 5
(define (func5) 5) ;; Función que devuelve 5
num5    
func5   ;; No estoy llamando a la función, faltan paréntesis
(func5) ;; Llamada a la función

;; Las funciones pueden recibir parámetros
(define (func-suma x y) (+ x y))
(func-suma 5 10)

;; Las funciones son elementos de primera clase, lo que significa
;; que pueden pasarse como parámetro a otras funciones, puede
;; haber funciones que devuelvan nuevas funciones y se pueden
;; asignar a variables. Ejemplo de función que recibe otra función
;; como parámetro:
(define (aplicar-f f x y) (f x y))
(aplicar-f + 5 10) ;; Le pasamos la función + como parámetro para que la aplique

;; La ausencia de asignaciones en programación funcional (aunque Racket la soporta)
;; significa que las funciones no modifican las variables, si no que devuelven
;; un nuevo valor con la modificación, manteniendo el original intacto.
;; Por ejemplo, con la listas y la función list-set:
(define lista '(1 2 3 4 5))
lista ;; Se imprime la lista original
(list-set lista 0 10) ;; Devuelve la lista con el cambio
lista ;; La lista original sigue intacta

;; Las funciones cuyo id tiene varias palabras,
;; las separamos con -, por ejemplo func-if
;; como en esta función de abajo
(define (func-if x)
  (if (< x 5)
      "x menor 5"
      (if (< x 10)
          "x menor 10"
          "x mayor que 10")))
       
(func-if 1)
(func-if 6)
(func-if 11)

;; Para if anidados es mejor usar la función cond,
;; ya que queda un código más legible y elegante
(define (func-cond x)
  (cond
    [(< x 5) "x menor que 5"]
    [(< x 10) "x menor que 10"]
    [else "x mayor que 10"]))

(func-cond 1)
(func-cond 6)
(func-cond 11)

;; No tenemos bucles, en lugar usamos funciones recursivas
;; Podemos definir una función dentro de otra, algo habitual
;; en funciones recursivas para no tener que pasar parámetros
;; inecesarios a la función principal
(define (lista-sumar lista)  
  (define (lista-sumar-rec num lista)
    (cond [(empty? lista) num]
          [else (+ num (lista-sumar-rec (first lista) (rest lista)))]))
  (lista-sumar-rec 0 lista))