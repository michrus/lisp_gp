#lang scheme

; ------ HELPER FUCTIONS ------

; gets random element from list provided as argument
; args:
;     elements        - list of elements to be randomly chosen from
;     [start-index]   - lower bound index for random picking (default: 0)
;     [start-index]   - upper bound index for random picking (default: length of elements)
(define (get-random-element elements [start-index 0] [end-index (length elements)])
  (list-ref elements (random start-index end-index)))

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
        (f (append result (list (get-input-symbol i))) (sub1 i))
     )
    )
  )

; returns part of the list from first element to the specified element (including)
; args:
;     lst    - list
;     n      - index of element
(define (list-reverse-tail lst n)
  (cond
    [(or (> n (- (length lst) 1)) (< n 0))
     (error (string-append "Index "
                           (number->string n)
                           " out of range of "
                           (number->string (sub1 (length lst)))))]
    [else (reverse (list-tail (reverse lst) (- (length lst) n 1)))]
  )
)

; reverse cdr - returns list without last element
(define (rcdr lst)
  (reverse (cdr (reverse lst)))
)

; reverse car - returns last element of list
(define (rcar lst)
  (car (reverse lst))
)


; inserts element into a list at a specified position
(define (list-insert lst element n)
  (append (rcdr (list-reverse-tail lst n))
          (list element)
          (cdr (list-tail lst n))
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

; mutation consts
(define default-mutation-type 'mutate-terminator)

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
             (random-element (append program (list (get-terminator))) (sub1 i))
             )
         )
     )
  )
)

(define (get-population)
  (let f ([result '()] [i population-size])
    (if (zero? i)
        result
        (f (append result (list (get-random-program))) (sub1 i))
        )
   )
)

; ------ PROGRAM MODIFICATION ------
; Crossovers, mutations

(define (mutate-subtree program)
  (list-insert program
               (get-random-program)
               (random 1 (length program)))
  )

(define (mutate-terminator program)
  (let f ([subtree program])
    (let ([mutation-point (random 1 (length subtree))])
      (if (list? (list-ref subtree mutation-point))
          (list-insert subtree
                       (f (list-ref subtree mutation-point))
                       mutation-point
                       )
          (let g ([new-element (get-terminator)])
            (if (eq? (list-ref subtree mutation-point) new-element)
                (g (get-terminator))
                (list-insert subtree
                             new-element
                             mutation-point)
                )
            )
          )
      )
    )
  )

(define (mutate program [mutation-procedure (eval default-mutation-type)])
  (mutation-procedure program)
  )