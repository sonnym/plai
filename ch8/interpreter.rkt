#lang plai

;; define CFAE/L, CFAE/L-Value, Env
(define-type CFAE/L
  [num (n number?)]
  [add (lhs CFAE/L?) (rhs CFAE/L?)]
  [id (name symbol?)]
  [fun (param symbol?) (body CFAE/L?)]
  [app (fun-expr CFAE/L?) (arg-expr CFAE/L?)])

(define-type CFAE/L-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body CFAE/L?)
            (env Env?)]
  [exprV (expr CFAE/L?)
         (env Env?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?) (value CFAE/L-Value?) (env Env?)])

;; parse : sexp -> CFAE/L
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

;; interp : CFAE/L -> number
(define (strict-interp expr env)
  (strict (interp expr env)))

;; interp : CFAE/L -> CFAE/L-Value
(define (interp expr env)
  (type-case CFAE/L expr
    [num (n) (numV n)]
    [add (l r) (num+ (interp l env) (interp r env))]
    [id (v) (lookup v env)]
    [fun (bound-id bound-body)
         (closureV bound-id bound-body env)]
    [app (fun-expr arg-expr)
         (local ([define fun-val (strict (interp fun-expr env))]
                 [define arg-val (exprV arg-expr env)])
           (interp (closureV-body fun-val)
                   (aSub (closureV-param fun-val)
                         arg-val
                         (closureV-env fun-val))))]))

;; strict : CFAE/L-Value -> CFAE/L-Value (excluding exprV)
(define (strict e)
  (type-case CFAE/L-Value e
    [exprV (expr env)
           (strict (interp expr env))]
    [else e]))

;; num+ : numV numV -> numV
(define (num+ n1 n2)
  (numV (+ (numV-n (strict n1)) (numV-n (strict n2)))))

;; lookup : symbol Env -> CFAE/L-Value
(define (lookup name env)
  (type-case Env env
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-env)
      (if (symbol=? bound-name name)
          bound-value
          (lookup name rest-env))]))

;;
(test (interp (parse '{with {x 3} x}) (mtSub))
      (exprV (num 3) (mtSub)))

(test (strict-interp (parse '{with {x 3} x}) (mtSub))
      (numV 3))

(test (interp (parse '{with {x 3} {+ x x}}) (mtSub)) (numV 6))

(test (interp (parse '{with {f {undef x}} 4}) (mtSub)) (numV 4))

(test (num+ (numV 1) (numV 2)) (numV 3))
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
      (closureV 'y (add (id 'x) (id 'y)) (aSub 'x (exprV (num 3) (mtSub)) (mtSub))))
