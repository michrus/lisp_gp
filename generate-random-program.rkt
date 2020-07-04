#lang scheme

; DATA
; Data for function y = 5.6x^3 + 1.43x^2 + 7
; first column is x, second is y
(define X '(-10.0 -7.7 -5.5 -3.3 -1.1 1.1 3.3 5.5 7.7 10.0))
(define Y '(-5450.0 -2541.3 -909.1 -184.5 1.1 16.4 230.3 1011.3 2728.3 5750.0))

; ------ GLOBAL CONSTS ------
; input consts
(define inputs-count 1)

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

; fitness const
(define fitness-constant 1000)

; genetic operations consts
(define reproduction-probability 0.47)
(define crossover-probability 0.4)
(define mutation-probability 0.1)
(define architecture-mutation-probability 0.03)

(define best-always-reproduce #t)

; number of iterations
(define gp-iterations 1000)

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
  (let f ([result '()] [i 0])
    (if (= i inputs-count)
        result
        (f (append result (list (get-input-symbol i))) (add1 i))
     )
    )
  )

(define input-symbols (get-input-symbol-list inputs-count))

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

; get population of random programs
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
(define (mutate program)
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

; mutate program, adding sub-program at random point
; args:
;     program    - program list
(define (mutate-architecture program)
  (append (list (car program))
   (let ([mutation-point (random)]
        [node-probability (/ 1 (count-nodes (cdr program)))])
    (let f ([subtree (cdr program)]
            [probability-tree (get-node-probabilities (cdr program) node-probability)]
            [i 0]
            [prev-prob 0])
      (let ([element (list-ref subtree i)]
            [element-probability (list-ref probability-tree i)])
        (cond
          [(<= mutation-point element-probability)
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
                  (get-random-program)
                  (list-insert subtree
                               (get-random-program)
                               i))])]
          [else
           (f subtree
              probability-tree
              (add1 i)
              (list-ref probability-tree i))])
        )
  ))))

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

; insert element at given point in program
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

; perform crossover between two programs
(define (crossover programA programB)
  (let ([crossover-pointA (random)]
        [crossover-pointB (random)])
;    (list
     (program-insert programA
                     (find-subtree programB crossover-pointB)
                     crossover-pointA)
;     (program-insert programB
;                     (find-subtree programA crossover-pointA)
;                     crossover-pointB))
    )
  )

; ------ PROGRAM EVALUATION ------

; needed to properly run eval
(define eval-expr
  (let ((ns (make-base-namespace)))
    (lambda (expr) (eval expr ns))))

; bind single symbol to numerical value
(define (bind-symbol-value symbol value)
  (eval-expr (list 'define symbol value))
  )

; bind list of symbols to numerical values passed as list
(define (bind-input-values symbols values)
  (for-each bind-symbol-value symbols values)
  )

; returns outputs from program executed on input data
(define (execute-program program input-data)
  (let f ([x-local input-data]
          [y-pred '()])
    (cond
      [(not (null? x-local))
       (let ([input-vector (list (car x-local))])
         (bind-input-values input-symbols input-vector)
         (f (cdr x-local)
            (append y-pred (list (eval-expr program)))))]
      [else y-pred])))

(define (get-total-error y-pred target-data)
  (let f ([total-error 0]
          [y-pred-local y-pred]
          [target-data-local target-data])
    (cond
      [(null? y-pred-local) total-error]
      [else
       (let ([y-pred-value (car y-pred-local)]
             [target-value (car target-data-local)])
         (f (+ total-error (abs (- target-value y-pred-value)))
            (cdr y-pred-local)
            (cdr target-data-local)))])))

(define (get-average-error y-pred target-data)
  (let ([y-count (length y-pred)])
    (/ (get-total-error y-pred target-data) y-count)))

(define (get-fitness average-error)
  (/ fitness-constant (+ 1 average-error)))

(define (get-population-fitness population input-data target)
  (let f ([i 0] [population-fitness '()])
    (if (= i (length population))
        population-fitness
        (f (add1 i) (append population-fitness
                            (list
                             (get-fitness
                              (get-average-error
                               (execute-program (list-ref population i) input-data)
                               target))))))))

(define (get-program-fitness-pairs program-population fitness-list)
  (map (lambda (a b) (list a b)) program-population fitness-list))

(define (sort-population program-fitness-pairs)
  (sort program-fitness-pairs (lambda (a b)
                                (< (cadr a)
                                   (cadr b)))))

(define (get-population-roulette program-fitness-pairs)
  (let ([total-fitness (apply + (map cadr program-fitness-pairs))])
    (let f ([pairs (sort-population program-fitness-pairs)] [offset 0] [population-roulette '()])
        (if (null? pairs)
            population-roulette
            (let* ([program (caar pairs)]
                   [program-fitness (cadr (car pairs))]
                   [program-probability (/ program-fitness total-fitness)]
                   [new-offset (+ offset program-probability)])
              (f (cdr pairs)
                 new-offset
                 (append population-roulette (list (list program new-offset)))))))))

(define (select-program-probabilistically program-fitness-pairs)
  (let ([population-roulette (get-population-roulette program-fitness-pairs)]
        [probability-point (random)])
    (let f ([pairs population-roulette])
      (if (null? pairs)
          (car (rcar program-fitness-pairs))
          (let* ([program-probability-pair (car pairs)]
                  [program (car program-probability-pair)]
                  [program-probability (cadr program-probability-pair)])
             (if (<= probability-point program-probability)
                 program
                 (f (cdr pairs))))))))

(define (select-two-programs-probabilistically program-fitness-pairs)
  (let ([first (select-program-probabilistically program-fitness-pairs)]
        [second (select-program-probabilistically program-fitness-pairs)])
    (list first second)))

; genetic operations consts
(define genetic-operations (list (list (list
                                        "Reproduction"
                                        (lambda (program-fitness-pairs)
                                         (select-program-probabilistically
                                          program-fitness-pairs)))
                                       reproduction-probability)
                                 (list (list
                                        "Crossover"
                                        (lambda (program-fitness-pairs)
                                         (apply crossover
                                                (select-two-programs-probabilistically
                                                 program-fitness-pairs))))
                                       crossover-probability)
                                 (list (list
                                        "Mutation"
                                        (lambda (program-fitness-pairs)
                                         (mutate (select-program-probabilistically
                                                  program-fitness-pairs))))
                                       mutation-probability)
                                 (list (list
                                        "Architecture mutation"
                                        (lambda (program-fitness-pairs)
                                         (mutate-architecture (select-program-probabilistically
                                                               program-fitness-pairs))))
                                       architecture-mutation-probability)))

(define (get-random-genetic-operation)
  (let ([selected (select-program-probabilistically genetic-operations)])
    ;(displayln (car selected))
     (cadr selected)))

(define (get-new-population program-fitness-pairs [best-always-reproduce best-always-reproduce])
  (let f ([i (length program-fitness-pairs)]
          [new-population '()])
    (if (zero? i)
        new-population
        (f (sub1 i)
           (append new-population (list ((get-random-genetic-operation) program-fitness-pairs)))))))

(define (genetic-programming input-data target iterations)
  (let* ([result-population 
         (let f ([i 0]
                 [population (get-population)])
           (let* ([program-fitness-pairs (get-program-fitness-pairs population
                                                                    (get-population-fitness population
                                                                                            input-data
                                                                                            target))]
                  [best-pair (rcar program-fitness-pairs)]
                  [best-program (car best-pair)]
                  [best-fitness (cadr best-pair)]
                  )
             (displayln (string-append "Iteration: " (number->string i)))
             (displayln (string-append "Best fitness: " (number->string best-fitness)))
             ;(pretty-display population)
             (if (= i iterations)
                 population
                 (f (add1 i)
                    (get-new-population program-fitness-pairs)))))]
         [result-program (rcar result-population)])
    (displayln "Genetic Programming stopped")
    (displayln "Best Program:")
    (pretty-display result-program)
    (displayln "End population:")
    (pretty-display result-population)
    result-program))

; SETTINGS

; RUN GENETIC PROGRAMMING
(genetic-programming X Y gp-iterations)