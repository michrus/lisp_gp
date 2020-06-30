#lang scheme

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

; returns terminator element for program tree - either input variable or random constant
; args:
;     input-symbols        - list of symbols for input variables
;     const-min            - minimal value for random constant
;     const-max            - maximal value for random constant
;     const-probability    - probability of picking random constant [default: 0.5]
(define (get-terminator input-symbols const-min const-max [const-probability 0.5])
  (if (<= (random) const-probability)
      (+ (random const-min const-max) (random))
      (get-random-element input-symbols)
   )
  )

; generate random program tree
; args:
;     functions        - list of function symbols to construct program from
;     input-symbols    - list of symbols for input variables
;     const-min        - minimal value for random constant (for get-terminator)
;     const-max        - maximal value for random constant (for get-terminator)
;     min-sub-elements - minimal number of elements in each sub-tree
;     max-sub-elements - maximal number of elements in each sub-tree
;     sub-probability  - probability of generating sub-program instead of pcking terminator
(define (get-random-program
         functions
         input-symbols
         const-min
         const-max
         min-sub-elements
         max-sub-elements
         sub-probability)
  (append (list (get-random-element functions))
   (let random-element ([program '()] [i (random min-sub-elements max-sub-elements)])
     (if (< i 0)
         program
         (if (<= (random) sub-probability)
             (append program (list (get-random-program
                                    functions
                                    input-symbols
                                    const-min
                                    const-max
                                    min-sub-elements
                                    max-sub-elements
                                    sub-probability)))
             (random-element (append program (list (get-terminator input-symbols const-min const-max))) (- i 1))
             )
         )
     )
  )
)