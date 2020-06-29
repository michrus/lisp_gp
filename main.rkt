#lang scheme

(define bar '(+ - / * "(" ")") )
(define bar2 '(* "(" + 2 3 ")" 4) )
(define bar3 '(* ( + 2 3 ) 4) )

; gets random element from list passed as argument
(define (get-random-element elements)
  (list-ref elements (random 0 (length elements))))

;(list  (cons (get-random-element bar) '() ))

; creates list of given length consisting of random elements passed as argument
; args:
;     elements    list of possible elements to build result from
;     length      length of generated list
(define (get-elements-list elements length)
 (let add-element ([len length] [lst '()])
   (if (zero? len)
       lst
       (add-element (- len 1) (cons (get-random-element elements) lst))
       )
   )
)

; creates radom program of given maximal length, consisting of elements passed as argument
; args:
;     elements    list of possible elements to build result from
;     max-length  maximal length of generated program
(define (get-program elements max-length)
  (get-elements-list elements (random 1 (+ 1 max-length))))

(get-program bar 10)