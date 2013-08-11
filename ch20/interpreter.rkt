#lang plai

;; define KCFAE and KCFAE-Value
(define-type KCFAE
  [num (n number?)]
  [add (lhs KCFAE?) (rhs KCFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body KCFAE?)]
  [app (fun-expr KCFAE?) (arg-expr KCFAE?)]
  [if0 (cond-expr KCFAE?) (succ-expr KCFAE?) (fail-expr KCFAE?)])

(define-type KCFAE-Value
  [numV (n number?)]
  [closureV (p procedure?)])

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
        [(if0) [if0 (parse (second sexp))
                    (parse (third sexp))
                    (parse (fourth sexp))]]
        [else [app (parse (first sexp))
                   (parse (second sexp))]])]))

;; interp : KCFAE Env -> KCFAE-Value
(define (interp expr env)
  (type-case RCFAE expr
    [num (n) (numV n)]
    [add (l r) (num+ (interp l env) (interp r env))]
    [if0 (test truth falsity)
         (if (num-zero? (interp test env))
             (interp truth env)
             (interp falsity env))]
    [id (v) (lookup v env)]
    [fun (bound-id bound-body)
         (closureV (lambda (arg-val)
                     (interp bound-body
                             (aSub bound-id arg-val env))))]
    [app (fun-expr arg-expr)
         (local ([define fun-val (interp fun-expr env)]
                 [define arg-val (interp arg-expr env)])
           (unless (closureV? fun-val)
                   (error 'interp "function expression did not evaluate to a function ~v" fun-expr))
           ((closureV-p fun-val)
            arg-val))]))

;; num+ : numV numV -> numV
(define (num+ n1 n2) (numV (+ (numV-n n1) (numV-n n2))))

;; num-zero? : KCFAE-Value -> boolean
(define (num-zero? n) (zero? (numV-n n)))

;; lookup : symbol Env -> KCFAE-Value
(define (lookup name env)
  (env name))
