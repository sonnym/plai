#lang plai

;; define BCFAE, BCFAE-Value, and Env
(define-type BCFAE
  [num (n number?)]
  [add (lhs BCFAE?) (rhs BCFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body BCFAE?)]
  [app (fun-expr BCFAE?) (arg-expr BCFAE?)]
  [if0 (cond-expr BCFAE?) (succ-expr BCFAE?) (fail-expr BCFAE?)])

(define-type BCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body BCFAE?)
            (env Env?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?)
        (value BCFAE-Value?)
        (env Env?)])

;; parse : sexp -> BCFAE
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
        [else [app (parse (first sexp)) (parse (second sexp))]])]))

;; interp : BCFAE Env -> BCFAE-Value
(define (interp expr env)
  (type-case BCFAE expr
    [num (n) (numV n)]
    [add (l r) (num+ (interp l env) (interp r env))]
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
                         (closureV-env fun-val))))]))

;; num+ : numV numV -> numV
(define (num+ n1 n2) (numV (+ (numV-n n1) (numV-n n2))))

;; num-zero? : BCFAE-Value -> boolean
(define (num-zero? n) (zero? (numV-n n)))

;; lookup : symbol Env -> BCFAE-Value
(define (lookup name env)
  (type-case Env env
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-env)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name rest-env))]))

;;
