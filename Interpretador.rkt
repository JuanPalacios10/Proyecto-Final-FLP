#lang eopl
; Juan Miguel Palacios Doncel #2359321
; Nicolas Rodriguez Romero #2266071
; Jhon Alexis Ruiz Quiceno #2266014

(require rackunit)

(define especificacion-lexica
  '(
      (espacio-blanco (whitespace) skip)
      (comentario ("(*" (arbno (not #\newline)) "*)") skip)
      (identificador (letter (arbno (or letter digit "?" "$"))) symbol)
      (numero (digit (arbno digit)) number)
      (numero ("-" digit (arbno digit)) number)
      (numero (digit (arbno digit)"." digit (arbno digit)) number)
      (numero ("-" digit (arbno digit)"." digit (arbno digit)) number)
      (caracter ("'" (or letter digit) "'") string)
      (cadena ("\"" (arbno (or letter digit)) "\"") string)
  )
)

(define especificacion-gramatical
  '(
    (programa (expresion) a-program)
    (expresion (bool-expresion) a-bool-expresion)
    (expresion (identificador) id-exp)
    (expresion (numero) a-numero)
    (expresion (caracter) a-caracter)
    (expresion (cadena) a-cadena)
    (expresion ("ok") empty-exp)
    (expresion ("var" (separated-list identificador "=" expresion ",") "in" expresion "end") var-exp)
    (expresion ("let" (separated-list identificador "=" expresion ",") "in" expresion "end") let-exp)
    (expresion ("letrec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion) "in" expresion "end") letrec-exp)
    (expresion ("set" identificador ":=" expresion) set-exp)
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion (primitiva "(" (arbno expresion) ")") prim-exp)
    (expresion ("if" bool-expresion "then" expresion (arbno "elseif" bool-expresion "then" expresion) "else" expresion "end") if-exp)
    (expresion ("proc" "(" (separated-list identificador ",") ")" expresion) proc-exp)
    (expresion ("apply" identificador "(" (separated-list expresion ",") ")") apply-exp)
    (expresion ("for" identificador "=" expresion "to" expresion "do" expresion "end") for-exp)

    ;; Expresiones bool-expresion
    (bool-expresion ("true") true-exp)
    (bool-expresion ("false") false-exp)
    (bool-expresion (bool-primitiva "(" (separated-list expresion ",") ")") a-bool-primitiva)
    (bool-expresion (bool-oper "(" (separated-list bool-expresion ",") ")") a-bool-oper)

    ;; Expresiones bool-primitiva
    (bool-primitiva ("<") menor-prim)
    (bool-primitiva (">") mayor-prim)
    (bool-primitiva ("<=") menorigual-prim)
    (bool-primitiva (">=") mayorigual-prim)
    (bool-primitiva ("is") is-prim)

    ;;Expresiones bool-oper
    (bool-oper ("and") and-oper)
    (bool-oper ("or") or-oper)
    (bool-oper ("not") not-oper)

    ;;Expresiones Primivitas
    (primitiva ("+") suma-prim)
    (primitiva ("-") resta-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("%") mod-prim)
    (primitiva ("&") concat-prim)

  )
)

;;Creamos los datatypes automaticamente
(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

;;Definición para los valores
(define values?
  (lambda (value)
    #t
  )
)
;;ambientes
(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido
    (lids (list-of symbol?))
    (lvalue (list-of values?))
    (old-env ambiente?)
  )
  (ambiente-extendido-rec
    (procnames (list-of symbol?))
    (lidss (list-of (list-of symbol?)))
    (cuerpos (list-of expresion?))
    (old-env ambiente?)
  )
  (ambiente-extendido-ref
    (lids (list-of symbol?))
    (lvalue vector?)
    (old-env ambiente?)
  )
)

(define ambiente-inicial
  (ambiente-extendido '(x y z) '(4 2 5) (ambiente-vacio)))

; (define ambiente-extendido-recursivo
;   (lambda (procnames lidss cuerpos old-env)
;     (let
;         (
;            (vec-clausuras (make-vector (length procnames)))
;         )
;       (letrec
;         (
;           (amb (ambiente-extendido-ref procnames vec-clausuras old-env))
;           (obtener-clausuras
;             (lambda (lidss cuerpos pos)
;               (cond
;                 [(null? lidss) amb]
;                 [else
;                   (begin
;                     (vector-set! vec-clausuras pos
;                     (closure (car lidss) (car cuerpos) amb))
;                     (obtener-clausuras (cdr lidss) (cdr cuerpos) (+ pos 1))
;                   )
;                 ]
;                 )
;               )
;             )
;           )
;         (obtener-clausuras lidss cuerpos 0)
;       )
;     )
;   )
; )

(define check-env
  (lambda (amb var)
    (let
      (
        (ref (apply-env amb var))
      )
      (if (referencia? ref)
        (deref ref)
        ref
      ) 
    ) 
  )
)

; (define apply-env
;   (lambda (env var)
;     (cases ambiente env
;       (ambiente-vacio () (eopl:error "No se encuentra la variable " var))
;       (ambiente-extendido (lid lval old-env)
;         (letrec
;           (
;             (buscar-variable
;              (lambda (lid lval)
;                (cond
;                   [(null? lid) (check-env old-env var)]
;                   [(equal? (car lid) var) (car lval)]
;                   [else
;                   (buscar-variable (cdr lid) (cdr lval))
;                   ]
;                 )
;               )
;             )
;           )
;           (buscar-variable lid lval)
;         )
;       ) 
;       (ambiente-extendido-ref (lid vec old-env)
;         (letrec
;           (
;             (buscar-variable
;              (lambda (lid vec pos)
;                (cond
;                   [(null? lid) (check-env old-env var)]
;                   [(equal? (car lid) var) (a-ref pos vec)]
;                   [else
;                   (buscar-variable (cdr lid) vec (+ pos 1))]
;                 )
;               )
;             )
;           )
;           (buscar-variable lid vec 0)
;         )
;       )
;     )
;   )
; )

(define apply-env
  (lambda (env var)
    (cases ambiente env
      (ambiente-vacio () (eopl:error "No se encuentra la variable " var))
      (ambiente-extendido (lid lval old-env)
        (letrec
          (
            (buscar-variable
             (lambda (lid lval)
               (cond
                  [(null? lid) (apply-env old-env var)]
                  [(equal? (car lid) var) (car lval)]
                  [else
                  (buscar-variable (cdr lid) (cdr lval))
                  ]
                )
              )
            )
          )
          (buscar-variable lid lval)
        )
      ) 
      (ambiente-extendido-rec (procnames lidss cuerpos old-env)
        (let 
          (
            (pos (list-find-position var procnames))
          )
            (if (number? pos)
              (closure 
                (list-ref lidss pos)
                (list-ref cuerpos pos) 
                env
              )
              (apply-env old-env var)
            )
        )
      )
      (ambiente-extendido-ref (lid vec old-env)
        (letrec
          (
            (buscar-variable
             (lambda (lid vec pos)
               (cond
                  [(null? lid) (apply-env old-env var)]
                  [(equal? (car lid) var) (a-ref pos vec)]
                  [else
                  (buscar-variable (cdr lid) vec (+ pos 1))]
                )
              )
            )
          )
          (buscar-variable lid vec 0)
        )
      )
    )
  )
)

(define set-env!
  (lambda (env var val)
    (cases ambiente env
      (ambiente-vacio () (eopl:error "No se encuentra la variable " var))
      (ambiente-extendido (lids lvalues old-env)
        (letrec
          (
            (buscar-variable (lambda (lids lvalues)
              (cond
                [(null? lids) (set-env! old-env var val)]
                [(equal? (car lids) var)
                 (eopl:error "No se puede cambiar el valor de la variable" var)
                ]
                [else
                 (buscar-variable (cdr lids) (cdr lvalues))])))
          )
          (buscar-variable lids lvalues)
        )
      )
      (ambiente-extendido-rec (procnames lidss cuerpos old-env)
        (letrec
          (
            (buscar-variable (lambda (procnames)
              (cond
                [(null? procnames) (set-env! old-env var val)]
                [(equal? (car procnames) var)
                 (eopl:error "No se puede cambiar el valor de la variable" var)
                ]
                [else
                 (buscar-variable (cdr procnames))
                ]
              ))
            )
          )
          (buscar-variable procnames)
        )
      )
      (ambiente-extendido-ref (lids vec old-env)
        (setref! (apply-env env var) val)
      )
    )
  )
)

;;Definiciones para los procedimientos
(define-datatype procval procval?
  (closure (lid (list-of symbol?))
           (body expresion?)
           (amb-creation ambiente?)))

;;Referencias
(define-datatype referencia referencia?
  (a-ref (pos number?)
         (vec vector?)))

;;Extractor de referencias
(define deref
  (lambda (ref)
    (primitiva-deref ref)))

(define primitiva-deref
  (lambda (ref)
    (cases referencia ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

;;Asignación/cambio referencias
(define setref!
  (lambda (ref val)
    (primitiva-setref! ref val)))

(define primitiva-setref!
  (lambda (ref val)
    (cases referencia ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

;;Evaluar programa
(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (a-program (exp) (evaluar-expresion exp ambiente-inicial))
    ))
)

(define evaluar-expresion
  (lambda (exp amb)
    (cases expresion exp
      (a-bool-expresion (bool-exp) (evaluar-bool-expresion bool-exp amb))
      (a-numero (n) n)
      (id-exp (v) (check-env amb v))
      (a-caracter (c) (string-ref c 1))
      (a-cadena (str) (substring str 1 (- (string-length str) 1)))
      (empty-exp () empty)
      ;; Estructura var
      (var-exp (lids lexps body)
        (let
          (
            (lvalues (map (lambda (x) (evaluar-expresion x amb)) lexps))
          )
          (evaluar-expresion body (ambiente-extendido-ref lids (list->vector lvalues) amb))
        )
      )
      ;; Estructura let
      (let-exp (lids lexps body)
        (let
          (
            (lvalues (map (lambda (x) (evaluar-expresion x amb)) lexps))
          )
          (evaluar-expresion body (ambiente-extendido lids lvalues amb))
        )
      )
      ;; Estructura letrec
      (letrec-exp (procnames idss cuerpos cuerpo-letrec)
        (evaluar-expresion cuerpo-letrec
          (ambiente-extendido-rec procnames idss cuerpos amb)
        )
      )
      ;; Estructura set
      (set-exp (var exp)
        (let
          (
            (val (evaluar-expresion exp amb))
          )
          (set-env! amb var val)
        )
      )
      ;; Estructura begin
      (begin-exp (exp lexp)
        (if (null? lexp)
          (evaluar-expresion exp amb)
          (begin
            (evaluar-expresion exp amb)
            (letrec
                (
                  (evaluar-begin (lambda (lexp)
                    (cond
                      [(null? (cdr lexp)) (evaluar-expresion (car lexp) amb)]
                      [else
                        (begin
                          (evaluar-expresion (car lexp) amb)
                          (evaluar-begin (cdr lexp))
                        )
                      ]
                    ))
                  )
                )
              (evaluar-begin lexp)
            )
          )
        )
      )
      ;; Estructura primitiva
      (prim-exp (prim lexp) 
        (let
          (
            (lvalues (map (lambda (x) (evaluar-expresion x amb)) lexp))
          )
          (evaluar-prim-expresion prim lvalues)
        )
      )
      ;; Estructura if
      (if-exp (cond1 es-true lifs lexps es-false)
        (cond
          [(evaluar-bool-expresion cond1 amb) (evaluar-expresion es-true amb)]
          [(null? lifs) (evaluar-expresion es-false amb)]
          [else
            (evaluar-expresion (if-exp (car lifs) (car lexps) (cdr lifs) (cdr lexps) es-false) amb)
          ]
        )
      )
      ;; Estructura proc
      (proc-exp (ids body) (closure ids body amb))
      ;; Estructura apply
      (apply-exp (procname lexps)
        (let
          (
            (lrands (map (lambda (x) (evaluar-expresion x amb)) lexps))
            (procV (check-env amb procname))
          )
          (if (procval? procV)
            (cases procval procV
            (closure (lid body old-env)
              (if (= (length lid) (length lrands))
                (evaluar-expresion body (ambiente-extendido-ref lid (list->vector lrands) old-env))
                (eopl:error "El número de argumentos no es correcto, debe enviar" (length lid)  " y usted ha enviado" (length lrands))
              )
            ))
            (eopl:error "No puede evaluarse algo que no sea un procedimiento" procV) 
          )
        )
      )
      ;; Estructura for
      (for-exp (id valor cond-parada hacer)
        (letrec
          (
            (lvalues (list (evaluar-expresion valor amb)))
            (parada (evaluar-expresion cond-parada amb))
            (ide (ambiente-extendido-ref (list id) (list->vector lvalues) amb))
            (iterador (lambda (ide val parada hacer)
              (let* 
                (
                  [nuevo-amb (ambiente-extendido-ref (list id) (list->vector val) amb)]
                  [resultado-hacer (evaluar-expresion hacer nuevo-amb)]
                  [nuevo-val (+ 1 (car val))] 
                )
                (if (> nuevo-val parada)
                  resultado-hacer
                  (iterador ide (list nuevo-val) parada hacer)
                )
              )
            ))
          )
          (iterador ide lvalues parada hacer)
        )
      )
    )
  )
)

(define evaluar-bool-expresion
  (lambda (exp amb)
    (cases bool-expresion exp
      (true-exp () #true)
      (false-exp () #false)
      (a-bool-primitiva (bool-prim exps)
        (let 
          (
            (lvals (map (lambda (exp) (evaluar-expresion exp amb)) exps))
        
          )
          (evaluar-bool-primitiva bool-prim lvals)
        )
      )
      (a-bool-oper (bool-oper exps)
        (let
          (
            (lvals (map (lambda (exp) (evaluar-bool-expresion exp amb)) exps))
          )
          (evaluar-bool-oper bool-oper lvals)
        )
      )
    )
  )
)

; (define comparar-prim
;   (lambda (lvals prim)
;     (letrec
;       (
;         (menor (lambda (lvals)
;           (cond
;             [(null? (cdr lvals)) #true]
;             [(null? (cddr lvals)) (prim (car lvals) (cadr lvals))]
;             [else (and (prim (car lvals) (cadr lvals)) (menor (cdr lvals)))]
;           )
;         ))
;       )
;       (menor lvals)
;     )
;   )
; )

(define evaluar-bool-primitiva
  (lambda (prim lvals)
    (cases bool-primitiva prim
      (menor-prim () (apply < lvals))
      (mayor-prim () (apply > lvals))
      (menorigual-prim () (apply <= lvals))
      (mayorigual-prim () (apply >= lvals))
      (is-prim () (apply = lvals))
    )
  )
)

(define evaluar-prim-expresion
  (lambda (prim lvalues)
    (cases primitiva prim
      (suma-prim () (apply + lvalues))
      (resta-prim () (apply - lvalues))
      (mult-prim () (apply * lvalues))
      (mod-prim () (apply modulo lvalues))
      (concat-prim () (apply string-append lvalues))
    )
  )
)

(define list-find-position
  (lambda (simbolo lista)
    (letrec 
      (
        (aux 
          (lambda (lst pos)
            (cond
              [(null? lst) (eopl:error "No se encuentra el simbolo" simbolo)]                   
              [(equal? simbolo (car lst)) pos]     
              [else (aux (cdr lst) (+ pos 1))]
            )
          )
        )
      )
      (aux lista 0)
    )
  )
)

(define (andmap f lst)
  (if (null? lst)
    #t
    (if (f (car lst))
      (andmap f (cdr lst))
      #f
    )
  )
)

(define (ormap f lst)
  (if (null? lst)
    #f
    (if (f (car lst))
      #t
      (ormap f (cdr lst))
    )
  )
)

(define identity (lambda (x) x))

(define evaluar-bool-oper
  (lambda (oper lvals)
    (cases bool-oper oper
      (and-oper () (andmap identity lvals))
      (or-oper () (ormap identity lvals))
      (not-oper () 
        (if (= (length lvals) 1)
          (not (car lvals))
          (eopl:error "La operación not espera un unico valor")
        )
      )
    )
  )
)

;;scan&parse para las pruebas
(define scan&parse
  (sllgen:make-string-parser especificacion-lexica especificacion-gramatical))


;;Interpretador
(define interpretador
  (sllgen:make-rep-loop "-->" evaluar-programa
                        (sllgen:make-stream-parser
                         especificacion-lexica especificacion-gramatical)))

(provide (all-defined-out)) 

;; (interpretador)
