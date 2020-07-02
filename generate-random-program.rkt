#lang scheme

; ------ HELPER FUCTIONS ------

; counts number of nodes in list, accounting for nested elements
; args:
;     program                 - program list
;     [discount-functions]    - should be 0 or 1, in which case only terminators will be counted
(define (count-nodes program [discount-functions 0])
   (- (let f ([subtree program]
           [count 0])
     (if (null? subtree)
         count
         (f (cdr subtree)
            (if (list? (car subtree))
                (- (f (car subtree) count) discount-functions)    
                (add1 count)
                )
            )
         )
     ) discount-functions)
  )

; counts number of terminator nodes in list, accounting for nested elements
; args:
;     program                 - program list
(define (count-terminators program)
  (count-nodes program 1)
  )

; counts probability of sub-list, used for node probability calculation
; args:
;     top-subtree         - list to count probability of
;     node-probability    - probability of single element
;     [discount-functions]    - should be 0 or 1, in which case only terminators will be counted
(define (get-subtree-probability top-subtree node-probability [discount-functions 0])
  (let f ([subtree top-subtree]
          [i discount-functions]
          [count 0])
    (if (= i (length subtree))
         count
         (if (list? (list-ref subtree i))
             (f subtree
                (add1 i)
                (+ count (f (list-ref subtree i)
                            discount-functions
                            0
                            )
                       )
                )
             (f subtree
                (add1 i)
                (+ count node-probability)
                )
             )
         )
    )
  )

; constructs list of probabilities for program element's to be chosen, discounts root element.
; args:
;     program    - program list
(define (get-node-probabilities program [node-probability (/ 1 (count-nodes program))] [start-offset 0])
  (let f ([offset start-offset]
          [subtree program]
          [probability-tree '()]
          [i 0])
    (if (= i (length subtree))
        probability-tree
        (let ([new-offset
               (if (list? (list-ref subtree i))
                   (+ offset (get-subtree-probability (list-ref subtree i) node-probability))
                   (+ offset node-probability)
                   )])
          (f new-offset
             subtree
             (append probability-tree (list new-offset))
             (add1 i)
             )
          )
        )
    )
  )

; gets random index for list
; args:
;     elements        - list of elements to be randomly chosen from
;     [start-index]   - lower bound index for random picking (default: 0)
(define (get-random-index elements [start-index 0])
  (random start-index (length elements))
  )

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

(define (list-merge lst element n)
  (append (rcdr (list-reverse-tail lst n))
          element
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
(define default-mutation-type 'mutate-prob)
(define mutate-subtree-recurse-probability 1.0)

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
   (let random-element ([program '()]
                        [i (random min-sub-elements max-sub-elements)])
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

; MUTATION
; mutate program, chosing mutation point based on probability of it's elements (which should be uniform)
; args:
;     program    - program list
(define (mutate-prob program)
  (append (list (car program))
   (let ([mutation-point (random)]
        [node-probability (/ 1 (count-nodes (cdr program)))])
    (let f ([subtree (cdr program)]
            [probability-tree (get-node-probabilities (cdr program) node-probability)]
            [i 0]
            [prev-prob 0])
      (let ([element (list-ref subtree i)])
        (cond
          [(<= mutation-point (list-ref probability-tree i))
           (cond
             [(list? element)
              (list-insert subtree
                           (f element
                              (get-node-probabilities element node-probability prev-prob)
                              0
                              (list-ref probability-tree i))
                           i)]
             [else
              (list-insert subtree
                           (if (and (= i 0)
                                    (not (equal? (cdr program) subtree)))
                               (let g ([new-element (get-random-element functions)])
                                 (if (eq? element new-element)
                                     (g (get-random-element functions))
                                     new-element
                                     ))
                               (let g ([new-element (get-terminator)])
                                 (if (eq? element new-element)
                                     (g (get-terminator))
                                     new-element
                                     ))
                               )
                           i)
                  ])]
          [else
           (f subtree
              probability-tree
              (add1 i)
              (list-ref probability-tree i))])
        )
  ))))

; mutate program using mutation procedure of choice
; args:
;     program                - program list
;     [mutation-procedure]   - procedure used for mutation (default: take from global constant)
(define (mutate program [mutation-procedure (eval default-mutation-type)])
  (mutation-procedure program)
  )

; CROSSOVER

(define (find-subtree program probabiity-point)
  (let ([node-probability (/ 1 (count-nodes (cdr program)))])
    (let f ([subtree (cdr program)]
            [probability-tree (get-node-probabilities (cdr program) node-probability)]
            [i 0]
            [prev-prob 0])
      (let ([element (list-ref subtree i)]
            [element-probability (list-ref probability-tree i)])
        (cond
          [(<= probabiity-point element-probability)
           (cond
             [(list? element)
              (f element
                 (get-node-probabilities element node-probability prev-prob)
                 0
                 element-probability)]
             [else
              (if (and (= i 0)
                       (not (equal? (cdr program) subtree)))
                  subtree
                  element
                  )
              ])]
          [else
           (f subtree
              probability-tree
              (add1 i)
              element-probability)])
        )
      )))

(define (program-insert program insert-element probabiity-point)
  (append (list (car program))
          (let ([node-probability (/ 1 (count-nodes (cdr program)))])
            (let f ([subtree (cdr program)]
                    [probability-tree (get-node-probabilities (cdr program) node-probability)]
                    [i 0]
                    [prev-prob 0])
              (let ([element (list-ref subtree i)]
                    [element-probability (list-ref probability-tree i)])
                (cond
                  [(<= probabiity-point element-probability)
                   (cond
                     [(list? element)
                      (list-insert subtree
                                   (f element
                                      (get-node-probabilities element node-probability prev-prob)
                                      0
                                      element-probability)
                                   i)]
                     [else
                      (if (and (= i 0)
                               (not (equal? (cdr program) subtree)))
                          insert-element
                          (list-insert subtree
                                       insert-element
                                       i)
                          )
                      ])]
                  [else
                   (f subtree
                      probability-tree
                      (add1 i)
                      element-probability)])
                )
              ))))

;(define (crossover programA programB)
;  (let ([crossover-pointA (get-random-index programA 1)]
;        [crossover-pointB (get-random-index programB 1)])
;    
;    )
;  )