#lang eopl
; Juan Miguel Palacios Doncel #2359321
; Nicolas Rodriguez Romero #2266071
; Jhon Alexis Ruiz Quiceno #2266014

(require rackunit "interpretador.rkt")

(define begin-exp
  (scan&parse
    "begin  +(10 2); 5  end"
  )
)
(define expected_begin
  '1
)

(check-equal?  (evaluar-programa begin-exp) expected_begin)