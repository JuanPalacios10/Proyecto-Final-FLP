#lang eopl
; Juan Miguel Palacios Doncel #2359321
; Nicolas Rodriguez Romero #2266071
; Jhon Alexis Ruiz Quiceno #2266014

(require rackunit "interpretador.rkt")

<<<<<<< Updated upstream
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
=======
(define prueba-letrec1
  (scan&parse
    "letrec fact(n) = if is(n, 0) then 1 else *(n apply fact(-(n 1))) end in apply fact(5) end"
  )
)


(check-equal? (evaluar-programa prueba-letrec1) 120)

(define prueba-letrec2
  (scan&parse
    "letrec mult(x, y) = if is(y, 0) then 0 else +(x apply mult(x, -(y 1))) end in apply mult(4, 5) end"
  )
)

(check-equal? (evaluar-programa prueba-letrec2) 20)

(define prueba-proc1
  (scan&parse
    "let suma = proc(x, y) +(x y) in apply suma(3, 4) end"
  )
)

(check-equal? (evaluar-programa prueba-letrec2) 20)

(define prueba-proc2 
  (scan&parse
    "let sumaPrimeros = proc(n) var suma = 0 in for i = 1 to n do begin set suma := +(suma i); suma end end end in apply sumaPrimeros(4) end"
  )
)

(check-equal? (evaluar-programa prueba-proc2) 10)

(define prueba-fibo
  (scan&parse
    "letrec fibo(n) = if is(n, 0) then 0 elseif is(n, 1) then 1 else +(apply fibo(-(n 1)) apply fibo(-(n 2))) end in apply fibo(5) end"
  )
)

(check-equal? (evaluar-programa prueba-fibo) 5)

(define prueba-string
  (scan&parse
    "let mostrarEdad = proc(edad) if >(edad, 18) then \"Eres mayor de edad\" elseif and(>=(edad, 0), <=(edad, 18)) then \"Eres menor de edad\" else \"Edad invalida\" end in apply mostrarEdad(10) end"
  )
)

(check-equal? (evaluar-programa prueba-string) "Eres menor de edad")
>>>>>>> Stashed changes
