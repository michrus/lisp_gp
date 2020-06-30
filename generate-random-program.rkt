#lang scheme

; ------ HELPER FUCTIONS ------

; gets random element from list provided as argument
; args:
;     elements    - list of elements to be randomly chosen from
(define (get-random-element elements)
  (list-ref elements (random 0 (length elements))))

; construct symbol for input variable
; args:
;     number    - input variable number
(define (get-input-symbol number)
  (string->symbol (string-append "x" (number->string number)))
  )

; gets list of input variable symbols
; args:
;     inputs-count    - number of input variables
(define (get-input-symbol-list inputs-count)
  (let f ([result '()] [i inputs-count])
    (if (zero? i)
        result
        (f (append result (list (get-input-symbol i))) (- i 1))
     )
    )
  )

; ------ GLOBAL CONSTS ------
; input consts
(define inputs-count 2)
(define input-symbols (get-input-symbol-list inputs-count))

; terminator generation consts
(define terminator-random-min 1)
(define terminator-random-max 10)
(define terminator-const-probability 0.5)

; program generation consts
(define functions '(+ - * /))
(define min-sub-elements 2)
(define max-sub-elements 4)
(define sub-probability 0.2)
(define population-size 5)

; ------ PROGRAM GENERATION ------

; returns terminator element for program tree - either input variable or random constant
(define (get-terminator)
  (if (<= (random) terminator-const-probability)
      (+ (random terminator-random-min terminator-random-max) (random))
      (get-random-element input-symbols)
   )
  )

; generate random program tree
(define (get-random-program)
  (append (list (get-random-element functions))
   (let random-element ([program '()] [i (random min-sub-elements max-sub-elements)])
     (if (zero? i)
         program
         (if (<= (random) sub-probability)
             (append program (list (get-random-program)))
             (random-element (append program (list (get-terminator))) (- i 1))
             )
         )
     )
  )
)

(define (get-population)
  (let f ([result '()] [i population-size])
    (if (zero? i)
        result
        (f (append result (list (get-random-program))) (- i 1))
        )
   )
)

; ------ PROGRAM MODIFICATION ------
; Crossovers, mutations