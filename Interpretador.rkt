#lang eopl

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
    (expresion (identificador) var-exp)
    (expresion (numero) a-numero)
    (expresion (caracter) a-caracter)
    (expresion (cadena) a-cadena)
    (expresion ("ok") empty-exp)

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
  )
)

;;Creamos los datatypes automaticamente
(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

;;ambientes
(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido-ref
   (lids (list-of symbol?))
   (lvalue vector?)
   (old-env ambiente?)))

(define ambiente-extendido
  (lambda (lids lvalue old-env)
    (ambiente-extendido-ref lids (list->vector lvalue) old-env)))

(define ambiente-inicial
  (ambiente-extendido '(x y z) '(4 2 5) (ambiente-vacio)))

(define apply-env
  (lambda (env var)
    (deref (apply-env-ref env var))))


(define apply-env-ref
  (lambda (env var)
    (cases ambiente env
      (ambiente-vacio () (eopl:error "No se encuentra la variable " var))
      (ambiente-extendido-ref (lid vec old-env)
        (letrec
          (
            (buscar-variable (lambda (lid vec pos)
              (cond
                [(null? lid) (apply-env-ref old-env var)]
                [(equal? (car lid) var) (a-ref pos vec)]
                [else
                  (buscar-variable (cdr lid) vec (+ pos 1)  )]
              ))
            )
          )
            (buscar-variable lid vec 0)
          )
                          
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
      (var-exp (v) (apply-env amb v))
      (a-caracter (c) (string-ref c 1))
      (a-cadena (str) (substring str 1 (- (string-length str) 1)))
      (empty-exp () empty)
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

;;Interpretador
(define interpretador
  (sllgen:make-rep-loop "-->" evaluar-programa
                        (sllgen:make-stream-parser
                         especificacion-lexica especificacion-gramatical)))

(interpretador)