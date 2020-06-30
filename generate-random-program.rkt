#lang scheme

(define (random-real min max [use-real #t])
  (+ (random min max)
     (if use-real
         (random)
         0
     )
  )
)

(define (get-random-element elements)
  (list-ref elements (random 0 (length elements))))

(define (get-input-symbol number)
  (string->symbol (string-append "x" (number->string number)))
  )

(define (get-input-symbol-list inputs-count)
  (let f ([result '()] [i inputs-count])
    (if (zero? i)
        result
        (f (append result (list (get-input-symbol i))) (- i 1))
     )
    )
  )

(define (get-terminator input-symbols const-min const-max [const-probability 0.5])
  (if (<= (random) const-probability)
      (+ (random const-min const-max) (random))
      (get-random-element input-symbols)
   )
  )

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

(define functions '(+ - * /))
(define terminators '(x1 x2 random-const))