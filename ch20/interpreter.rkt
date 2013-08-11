#lang plai

;; define KCFAE and KCFAE-Value
(define-type KCFAE
  [num (n number?)]
  [add (lhs KCFAE?) (rhs KCFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body KCFAE?)]
  [app (fun-expr KCFAE?) (arg-expr KCFAE?)]
  [bindcc (cont-var symbol?) (body KCFAE?)]
  [if0 (cond-expr KCFAE?) (succ-expr KCFAE?) (fail-expr KCFAE?)])

(define-type KCFAE-Value
  [numV (n number?)]
  [closureV (p procedure?)]
  [contV (p procedure?)])

;; Env predicate
(define (Env? x)
  (procedure? x))

;; mtSub : () -> Env
(define (mtSub)
  (lambda (name)
    (error 'lookup "no binding for identifier: ~s" name)))

;; aSub : symbol KCFAE-Value Env -> Env
(define (aSub bound-name bound-value env)
  (lambda (want-name)
    (cond
      [(symbol=? want-name bound-name) bound-value]
      [else (lookup want-name env)])))

;; aRecSub : symbol boxed-KCFAE-Value Env -> Env
(define (aRecSub bound-name boxed-bound-value env)
  (lambda (want-name)
    (cond
      [(symbol=? want-name bound-name)
        (unbox boxed-bound-value)]
      [else (lookup want-name env)])))

;; parse : sexp -> KCFAE
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
        [(bindcc) (bindcc (second sexp)
                          (parse (third sexp)))]
        [(if0) [if0 (parse (second sexp))
                    (parse (third sexp))
                    (parse (fourth sexp))]]
        [else [app (parse (first sexp))
                   (parse (second sexp))]])]))

;; interp : KCFAE Env -> KCFAE-Value
(define (interp expr env k)
  (type-case KCFAE expr
    [num (n) (k (numV n))]
    [add (l r) (interp l env
                       (lambda (lv)
                         (interp r env
                                 (lambda (rv)
                                   (k (num+ lv rv))))))]
    [if0 (test truth falsity)
         (interp test env
                 (lambda (tv)
                   (if (num-zero? tv)
                       (interp truth env k)
                       (interp falsity env k))))]
    [id (v) (k (lookup v env))]
    [fun (param body)
         (k (closureV (lambda (arg-val dyn-k)
                      (interp body (aSub param arg-val env) dyn-k))))]
    [bindcc (cont-var body) 
            (interp body
                    (aSub cont-var
                          (contV (lambda (val)
                                   (k val)))
                          env)
                    k)]
    [app (fun-expr arg-expr)
         (interp fun-expr env
                 (lambda (fun-val)
                   (interp arg-expr env
                           (lambda (arg-val)
                             (type-case KCFAE-Value fun-val
                               [closureV (c) (c arg-val k)]
                               [contV (c) (c arg-val)]
                               [else (error "not an applicable value")])))))]))

;; num+ : numV numV -> numV
(define (num+ n1 n2) (numV (+ (numV-n n1) (numV-n n2))))

;; num-zero? : KCFAE-Value -> boolean
(define (num-zero? n) (zero? (numV-n n)))

;; lookup : symbol Env -> KCFAE-Value
(define (lookup name env)
  (env name))

;;
(define (interp-test p v)
  (interp (parse p) (mtSub) (lambda (k) (test k v))))

(interp-test '5 (numV 5))
(interp-test '{+ 5 5} (numV 10))
(interp-test '{with {x {+ 5 5}} {+ x x}} (numV 20))

(interp-test '{with {x 0}
                {bindcc adder {+ 1 x}}} 
             (numV 1))

(interp-test '{bindcc k 3} (numV 3))
(interp-test '{bindcc k {k 3}} (numV 3))
(interp-test '{bindcc k {+ 1 {k 3}}} (numV 3))
(interp-test '{+ 1 {bindcc k {+ 1 {k 3}}}} (numV 4))

(interp-test '{{bindcc k 
                       {k {fun {dummy}
                               3}}}
               1729}
             (numV 3)) 

(interp-test '{bindcc k
                      {k
                       {k
                        {k 3}}}}
             (numV 3))

(interp-test '{{{bindcc k k}
                {fun {x} x}}
               3}
             (numV 3))

(interp-test '{{{{bindcc k k}
                 {fun {x} x}}
                {fun {x} x}}
               3}
             (numV 3))
