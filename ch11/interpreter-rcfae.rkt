#lang plai

;; define RCFAE and RCFAE-Value
(define-type RCFAE
  [num (n number?)]
  [add (lhs RCFAE?) (rhs RCFAE?)]
  [mult (lhs RCFAE?) (rhs RCFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body RCFAE?)]
  [app (fun-expr RCFAE?) (arg-expr RCFAE?)]
  [if0 (cond-expr RCFAE?) (succ-expr RCFAE?) (fail-expr RCFAE?)]
  [rec (name symbol?) (named-expr RCFAE?) (body RCFAE?)])

(define-type RCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body RCFAE?)
            (env Env?)])

;; Env predicate
(define (Env? x)
  (procedure? x))

;; mtSub : () -> Env
(define (mtSub)
  (lambda (name)
    (error 'lookup "no binding for identifier: ~s" name)))

;; aSub : symbol RCFAE-Value Env -> Env
(define (aSub bound-name bound-value env)
  (lambda (want-name)
    (cond
      [(symbol=? want-name bound-name) bound-value]
      [else (lookup want-name env)])))

;; aRecSub : symbol boxed-RCFAE-Value Env -> Env
(define (aRecSub bound-name boxed-bound-value env)
  (lambda (want-name)
    (cond
      [(symbol=? want-name bound-name)
        (unbox boxed-bound-value)]
      [else (lookup want-name env)])))

;; parse : sexp -> RCFAE
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
        [(*) (mult (parse (second sexp))
                   (parse (third sexp)))]
        [(fun) (fun (first (second sexp))
                    (parse (third sexp)))]
        [(with) (app (fun (first (second sexp))
                          (parse (third sexp)))
                     (parse (second (second sexp))))]
        [(if0) [if0 (parse (second sexp))
                    (parse (third sexp))
                    (parse (fourth sexp))]]
        [(rec) [rec (first (second sexp))
                    (parse (second (second sexp)))
                    (parse (third sexp))]]
        [else [app (parse (first sexp))
                   (parse (second sexp))]])]))

;; interp : RCFAE Env -> RCFAE-Value
(define (interp expr env)
  (type-case RCFAE expr
    [num (n) (numV n)]
    [add (l r) (num+ (interp l env) (interp r env))]
    [mult (l r) (num* (interp l env) (interp r env))]
    [if0 (test truth falsity)
         (if (num-zero? (interp test env))
             (interp truth env)
             (interp falsity env))]
    [id (v) (lookup v env)]
    [fun (bound-id bound-body)
         (closureV bound-id bound-body env)]
    [app (fun-expr arg-expr)
         (local ([define fun-val (interp fun-expr env)])
           (unless (closureV? fun-val)
                   (error 'interp "function expression did not evaluate to a function ~v" fun-expr))
           (interp (closureV-body fun-val)
                   (aSub (closureV-param fun-val)
                         (interp arg-expr env)
                         (closureV-env fun-val))))]
    [rec (bound-id named-expr bound-body)
         (interp bound-body
                 (cyclically-bind-and-interp bound-id
                                             named-expr
                                             env))]))

;; num+ : numV numV -> numV
(define (num+ n1 n2) (numV (+ (numV-n n1) (numV-n n2))))

;; num* : numV numV -> numV
(define (num* n1 n2) (numV (* (numV-n n1) (numV-n n2))))

;; num-zero? : RCFAE-Value -> boolean
(define (num-zero? n) (zero? (numV-n n)))

;; lookup : symbol Env -> RCFAE-Value
(define (lookup name env)
  (env name))

;; cyclically-bind-and-interp : symbol fun env -> env
(define (cyclically-bind-and-interp bound-name named-expr env)
  (local ([define value-holder (box (numV 1729))]
          [define new-env (aRecSub bound-name value-holder env)]
          [define named-expr-val (interp named-expr new-env)])
    (set-box! value-holder named-expr-val)
    new-env))

;;
(test (interp (parse '{if0 {+ 5 -5} 1 2}) (mtSub)) (numV 1))

(test (parse '{rec {fac {fun {n}
                          {if0 n
                               1
                               {* n {fac {+ n -1}}}}}}
                 {fac 5}})
      (rec 'fac
           (fun 'n (if0 (id 'n)
                        (num 1)
                        (mult (id 'n)
                              (app (id 'fac) (add (id 'n)
                                                  (num -1))))))
           (app (id 'fac) (num 5))))

(test (interp (parse '{rec {fac {fun {n}
                                  {if0 n
                                       1
                                       {* n {fac {+ n -1}}}}}}
                       {fac 5}})
              (mtSub))
      (numV 120))
