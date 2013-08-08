#lang plai

;; define FAE, FAE-Value, and DefrdSub
(define-type FAE
  [num (n number?)]
  [add (lhs FAE?) (rhs FAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body FAE?)]
  [app (fun-expr FAE?) (arg-expr FAE?)])

(define-type FAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body FAE?)
            (ds DefrdSub?)])

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value FAE-Value?) (ds DefrdSub?)])

;; parse : sexp -> FAE
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
        [(with) (app (fun (first (second sexp))
                          (parse (third sexp)))
                     (parse (second (second sexp))))]
        [else [app (parse (first sexp)) (parse (second sexp))]])]))

;; interp : FAE -> FAE-Value
(define (interp expr ds)
  (type-case FAE expr
    [num (n) (numV n)]
    [add (l r) (add-numbers (interp l ds) (interp r ds))]
    [id (v) (lookup v ds)]
    [fun (bound-id bound-body)
         (closureV bound-id bound-body ds)]
    [app (fun-expr arg-expr)
         (local ([define fun-val (interp fun-expr ds)])
           (unless (closureV? fun-val)
                   (error 'interp "function expression did not evaluate to a function ~v" fun-expr))
           (interp (closureV-body fun-val)
                   (aSub (closureV-param fun-val)
                         (interp arg-expr ds)
                         (closureV-ds fun-val))))]))

;; add-numbers : numV numV -> numV
(define (add-numbers x y) (numV (+ (numV-n x) (numV-n y))))

;; lookup : symbol DefrdSub -> FAE-Value
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-ds)
      (if (symbol=? bound-name name)
          bound-value
          (lookup name rest-ds))]))

;;
(test (add-numbers (numV 1) (numV 2)) (numV 3))
(test (interp (parse '{+ 1 2}) (mtSub)) (numV 3))
(test (interp (parse '{fun {x} x}) (mtSub)) (closureV 'x (id 'x) (mtSub)))

(test (interp (parse '{{{fun {x} x}
                        {fun {x} {+ x 5}}}
                       3}) (mtSub))
      (numV 8))

(test (interp (parse '{with {x 3}
                        {with {f {fun {y} {+ x y}}}
                          {with {x 5}
                            {f 4}}}})
              (mtSub)) (numV 7))

(test (interp (parse '{with {x 3} {fun {y} {+ x y}}}) (mtSub))
      (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))))

;; exercise 6.4.1
(test/exn (interp (parse '{{{fun {x} x} 5} 3}) (mtSub))
          "interp: function expression did not evaluate to a function (app (fun 'x (id 'x)) (num 5))")
