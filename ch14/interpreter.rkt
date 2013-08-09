#lang plai

;; define VCFAE, VCFAE-Value, Env, Store, and ValuexStore
(define-type VCFAE
  [num (n number?)]
  [add (lhs VCFAE?) (rhs VCFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body VCFAE?)]
  [app (fun-expr VCFAE?) (arg-expr VCFAE?)]
  [if0 (cond-expr VCFAE?) (succ-expr VCFAE?) (fail-expr VCFAE?)]
  [seqn (sequents list?)])

(define-type VCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body VCFAE?)
            (env Env?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?)
        (location number?)
        (env Env?)])

(define-type Store
  [mtSto]
  [aSto (location number?)
        (value VCFAE-Value?)
        (store Store?)])

(define-type ValuexStore
  [vxs (value VCFAE-Value?) (store Store?)])

;; parse : sexp -> VCFAE
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
        [(seqn) (seqn (map parse (cdr sexp)))]
        [else [app (parse (first sexp)) (parse (second sexp))]])]))

;; interp : VCFAE Env -> ValuexStore
(define (interp expr env store)
  (type-case VCFAE expr
    [num (n) (vxs (numV n) store)]
    [add (l r) 
         (type-case ValuexStore (interp l env store)
           [vxs (l-value l-store)
             (type-case ValuexStore (interp r env l-store)
               [vxs (r-value r-store)
                 (vxs (num+ l-value r-value)
                      r-store)])])]
    [if0 (test truth falsity)
         (type-case ValuexStore (interp test env store)
           [vxs (test-value test-store)
                (if (num-zero? test-value)
                    (interp truth env test-store)
                    (interp falsity env test-store))])]
    [id (v) (vxs (store-lookup (env-lookup v env) store) store)]
    [fun (bound-id bound-body)
         (vxs (closureV bound-id bound-body env) store)]
    [seqn (sequents)
          (apply-sequents sequents env store)]
    [app (fun-expr arg-expr)
         (type-case ValuexStore (interp fun-expr env store)
           [vxs (fun-value fun-store)
             (type-case ValuexStore (interp arg-expr env fun-store)
               [vxs (arg-value arg-store)
                 (local ([define new-loc (next-location arg-store)])
                   (unless (closureV? fun-value)
                           (error 'interp "function expression did not evaluate to a function ~v" fun-expr))
                   (interp (closureV-body fun-value)
                           (aSub (closureV-param fun-value)
                                 new-loc
                                 (closureV-env fun-value))
                           (aSto new-loc
                                 arg-value
                                 arg-store)))])])]))

;; num+ : numV numV -> numV
(define (num+ n1 n2) (numV (+ (numV-n n1) (numV-n n2))))

;; num-zero? : VCFAE-Value -> boolean
(define (num-zero? n) (zero? (numV-n n)))

;; env-lookup : symbol Env -> location
(define (env-lookup name env)
  (type-case Env env
    [mtSub () (error 'env-lookup "no binding for identifier ~s" name)]
    [aSub (bound-name bound-location rest-env)
          (if (symbol=? bound-name name)
              bound-location
              (env-lookup name rest-env))]))

;; store-lookup : location Store -> VCFAE-Value
(define (store-lookup loc-index sto)
  (type-case Store sto
    [mtSto () (error 'store-lookup "no value at location")]
    [aSto (location value rest-store)
          (if (= location loc-index)
              value
              (store-lookup loc-index rest-store))]))

;; next-location : Store -> number
(define (next-location sto)
  (+ 1 (current-location sto)))

;; current-location : Store -> number
(define (current-location sto)
  (type-case Store sto
    [mtSto () -1]
    [aSto (location value rest-store) 
      (local ([define other-loc (current-location rest-store)])
        (if (> location other-loc) location other-loc))]))

;; apply-sequents : list Env Store -> ValuexStore
(define (apply-sequents sequents env store)
  (type-case ValuexStore (interp (car sequents) env store)
    [vxs (sequent-value sequent-store)
      (if (empty? (cdr sequents))
          (vxs sequent-value sequent-store)
          (apply-sequents (cdr sequents) env sequent-store))]))
