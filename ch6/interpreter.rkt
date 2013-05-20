#lang plai

;; define FWAE type
(define-type FWAE
  [num (n number?)]
  [add (lhs FWAE?) (rhs FWAE?)]
  [with (name symbol?) (named-expr FWAE?) (body FWAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body FWAE?)]
  [app (fun-expr FWAE?) (arg-expr FWAE?)])

;; parse : sexp -> FWAE
;; to convert s-expressions into FWAEs
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp)
      (case (first sexp)
        [(+) (add (parse (second sexp))
                  (parse (third sexp)))]
        [(-) (add (parse (second sexp))
                  (parse (- (third sexp))))]
        [(fun) (fun (first (second sexp))
                    (parse (third sexp)))]
        [(with) (with (first (second sexp))
                      (parse (second (second sexp)))
                      (parse (third sexp)))]
        [else [app (parse (first sexp)) (parse (second sexp))]])]))

;; interp : FWAE -> FWAE
;; evaluates FWAE expressions by reducing them to their corresponding values
;; return values are either number or fun
(define (interp expr)
  (type-case FWAE expr
    [num (n) expr]
    [add (l r) (add-numbers (interp l) (interp r))]
    [with (bound-id named-expr bound-body)
          (interp (subst bound-body
                         bound-id
                         (interp named-expr)))]
    [id (v) (error 'interp "free identifier")]
    [fun (bound-id bound-body)
         expr]
    [app (fun-expr arg-expr)
         (local ([define fun-val (interp fun-expr)])
           (interp (subst (fun-body fun-val)
                          (fun-param fun-val)
                          (interp arg-expr))))]))

;; add-numbers : FWAE FWAE -> FWAE
(define (add-numbers x y) (num (+ (num-n x) (num-n y))))

;; subst : FWAE symbol FWAE -> FWAE
;; substitutes second argument with third argument in first argument
;; as per the rules of substitution; the resulting expression contains
;; no free instances of the second argument
(define (subst expr sub-id val)
  (type-case FWAE expr
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
    [id (v) (if (symbol=? v sub-id) val expr)]
    [fun (bound-id bound-body)
         (fun bound-id (subst bound-body sub-id val))]
    [app (fun-name arg-expr)
         (app (subst fun-name sub-id val) (subst arg-expr sub-id val))]))

;;
(test (add-numbers (num 1) (num 2)) (num 3))
(test (interp (parse '{+ 1 2})) (num 3))
(test (interp (parse '{fun {x} x})) (fun 'x (id 'x)))
(test (interp (parse '{{{fun {x} x}
                        {fun {x} {+ x 5}}}
                       3}))
      (num 8))
(test (interp (parse '{with {x 3} {fun {y} {+ x y}}}))
      (fun 'y (add (num 3) (id 'y))))
