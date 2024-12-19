#lang eopl
; Juan Miguel Palacios Doncel #2359321
; Nicolas Rodriguez Romero #2266071
; Jhon Alexis Ruiz Quiceno #2266014

(require rackunit "interpretador.rkt")

(define var_begin
  (scan&parse
    "var x = 1, y = 2, z = 3 in begin x end end"
  )
)
(define expected_var_begin
  '1
)

(check-equal?  (evaluar-programa var_begin) expected_var_begin)


(define var_begin_set
  (scan&parse
    "var x = 1, y = 2 in begin set x := 10; set y := 12; +(x y) end end"
  )
)

(define expected_var_begin_set
  '22
)

(check-equal?  (evaluar-programa var_begin_set) expected_var_begin_set)


(define var_for
  (scan&parse
    "var x = 10 in for x = 1 to 3 do begin x end end end"
  )
)

(define expected_var_for
  '3
)

(check-equal?  (evaluar-programa var_for) expected_var_for)

(define if_exp
  (scan&parse
    "if <(1,2) then 1 else 2 end"
  )
)

(define expected_if_exp
  '1
)

(check-equal?  (evaluar-programa if_exp) expected_if_exp)