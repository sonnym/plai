#lang plai

; define F1WAE and FunDef

(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?) (rhs F1WAE?)]
  [with (name symbol?) (named-expr F1WAE?) (body F1WAE?)]
  [if0 (n F1WAE?) (success-cond F1WAE?) (fail-cond F1WAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?) (arg F1WAE?)])

(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body F1WAE?)])

; parse : sexp -> F1WAE
; to convert s-expressions into F1WAEs

(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp)
      (case (first sexp)
        [(+) (add (parse (second sexp))
                  (parse (third sexp)))]
        [(with) (with (first (second sexp))
                      (parse (second (second sexp)))
                      (parse (third sexp)))]
        [else (app (first sexp) (parse (second sexp)))])]))

;; subst : F1WAE symbol F1WAE -> F1WAE
;; substitutes second argument with third argument in first argument
;; as per the rules of substitution; the resulting expression contains
;; no free instances of the second argument

(define (subst expr sub-id val)
  (type-case F1WAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [with (bound-id named-expr bound-body)
      (if (symbol=? bound-id sub-id)
          (with bound-id
                (subst named-expr sub-id val)
                bound-body)
          (with bound-id
                (subst named-expr sub-id val)
                (subst bound-body sub-id val)))]
    [if0 (cond-test success-expr fail-expr)
         (if0 (subst cond-test sub-id val)
              (subst success-expr sub-id val)
              (subst fail-expr sub-id val))]
    [id (v) (if (symbol=? v sub-id) val expr)]
    [app (fun-name arg-expr)
         (app fun-name (subst arg-expr sub-id val))]))

;; interp : F1WAE listof(fundef) -> number
;; evaluates F1WAE expressions by reducing them to their corresponding values

(define (interp expr fun-defs)
  (type-case F1WAE expr
    [num (n) n]
    [add (l r) (+ (interp l fun-defs) (interp r fun-defs))]
    [with (bound-id named-expr bound-body)
          (interp (subst bound-body 
                         bound-id
                         (num (interp named-expr fun-defs))
                  fun-defs))]
    [if0 (cond-test success-expr fail-expr)
         (if (equal? (interp cond-test fun-defs) 0)
             (interp success-expr fun-defs)
             (interp fail-expr fun-defs))]
    [id (v) (error 'interp "free identifier")]
    [app (fun-name arg-expr)
         (local ([define the-fun-def (lookup-fundef fun-name fun-defs)])
           (interp (subst (fundef-body the-fun-def)
                          (fundef-arg-name the-fun-def)
                          (num (interp arg-expr fun-defs)))
                   fun-defs))]))

;; lookup-fundef : symbol listof(FunDef) -> FunDef

(define (lookup-fundef fun-name fundefs)
  (cond
    [(empty? fundefs) (error fun-name "function not found")]
    [else (if (symbol=? fun-name (fundef-fun-name (first fundefs)))
      (first fundefs)
      (lookup-fundef fun-name (rest fundefs)))]))

;; test expected output

(test (interp (parse '{double {double 5}})
              (list (fundef 'double
                            'n
                            (add (id 'n) (id 'n)))))
      20)

;; exercies 4.1.1
;; the argument can be resolved through function applications
(test (interp (parse '{add-1 {add-2 1}})
              (list (fundef 'add-2
                            'n
                            (add (app 'add-1 (num 1)) (id 'n)))
                    (fundef 'add-1
                            'n
                            (add (num 1) (id 'n)))))
      4)


;; exercise 4.1.2
;; the body can be composed of other functions
(test (interp (parse '{quadruple 5})
              (list (fundef 'double
                            'n
                            (add (id 'n) (id 'n)))
                    (fundef 'quadruple
                            'n
                            (add (app 'double (id 'n)) (app 'double (id 'n))))))
      20)

;; exercise 4.3.1
(test (interp (parse '{add-up-to 100})
              (list (fundef 'add-up-to
                            'n
                            (if0 (id 'n)
                                 (num 0)
                                 (add (id 'n)
                                      (app 'add-up-to
                                           (add (num -1) (id 'n))))))))
      5050)
