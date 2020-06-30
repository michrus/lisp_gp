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

(define (get-random-program functions terminators min-sub-elements max-sub-elements sub-probability)
  (append (list (get-random-element functions))
   (let random-element ([program '()] [i (random min-sub-elements max-sub-elements)])
     (if (zero? i)
         program
         (if (<= (random) sub-probability)
             (append program (list (get-random-program functions terminators min-sub-elements max-sub-elements sub-probability)))
             (random-element (append program (list (get-random-element terminators))) (- i 1))
             )
         )
     )
  )
)

(define functions '(+ - * /))
(define terminators '(x1 x2 random-const))