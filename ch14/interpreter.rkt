#lang plai

;; define RVCFAE, RVCFAE-Value, Env, Store, and ValuexStore
(define-type RVCFAE
  [num (n number?)]
  [add (lhs RVCFAE?) (rhs RVCFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body RVCFAE?)]
  [app (fun-expr RVCFAE?) (arg-expr RVCFAE?)]
  [ref-app (fun-expr RVCFAE?) (arg-expr RVCFAE?)]
  [if0 (cond-expr RVCFAE?) (succ-expr RVCFAE?) (fail-expr RVCFAE?)]
  [setv (var symbol?) (value-expr RVCFAE?)]
  [seqn (sequents list?)])

(define-type RVCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body RVCFAE?)
            (env Env?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?)
        (location number?)
        (env Env?)])

(define-type Store
  [mtSto]
  [aSto (location number?)
        (value RVCFAE-Value?)
        (store Store?)])

(define-type ValuexStore
  [vxs (value RVCFAE-Value?) (store Store?)])

;; parse : sexp -> RVCFAE
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
        [(setv) (setv (second sexp)
                      (parse (third sexp)))]
        [(seqn) (seqn (map parse (cdr sexp)))]
        [else (local ([define arg-expr (second sexp)])
                (if (and (list? arg-expr) (symbol=? 'ref (first arg-expr)))
                    [ref-app (parse (first sexp)) (parse (second arg-expr))]
                    [app (parse (first sexp)) (parse arg-expr)]))])]))


;; interp : RVCFAE Env -> ValuexStore
(define (interp expr env store)
  (type-case RVCFAE expr
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
    [setv (var value)
         (type-case ValuexStore (interp value env store)
           [vxs (value-value value-store)
                (local ([define the-loc (env-lookup var env)])
                  (vxs value-value
                       (aSto the-loc value-value value-store)))])]
    [seqn (sequents)
          (apply-sequents sequents env store)]
    [app (fun-expr arg-expr)
         (type-case ValuexStore (interp fun-expr env store)
           [vxs (fun-value fun-store)
                (type-case ValuexStore (interp arg-expr env fun-store)
                  [vxs (arg-value arg-store)
                    (local ([define new-loc (next-location arg-store)])
                      (interp (closureV-body fun-value)
                              (aSub (closureV-param fun-value)
                                    new-loc
                                    (closureV-env fun-value))
                              (aSto new-loc
                                    arg-value
                                    arg-store)))])])]
    [ref-app (fun-expr arg-expr)
             (type-case ValuexStore (interp fun-expr env store)
               [vxs (fun-value fun-store)
                    (local ([define arg-loc (env-lookup (id-name arg-expr) env)])
                      (interp (closureV-body fun-value)
                              (aSub (closureV-param fun-value)
                                    arg-loc
                                    (closureV-env fun-value))
                              fun-store))])]))

;; num+ : numV numV -> numV
(define (num+ n1 n2) (numV (+ (numV-n n1) (numV-n n2))))

;; num-zero? : RVCFAE-Value -> boolean
(define (num-zero? n) (zero? (numV-n n)))

;; env-lookup : symbol Env -> location
(define (env-lookup name env)
  (type-case Env env
    [mtSub () (error 'env-lookup "no binding for identifier ~s" name)]
    [aSub (bound-name bound-location rest-env)
          (if (symbol=? bound-name name)
              bound-location
              (env-lookup name rest-env))]))

;; store-lookup : location Store -> RVCFAE-Value
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

;;
(test (vxs-value (interp (parse '{with {v 0}
                                   {with {f {fun {y}
                                              {setv y 5}}}
                                     {seqn {f v}
                                           v}}})
                         (mtSub) (mtSto)))
      (numV 0))

(test (vxs-value (interp (parse '{with {v 0}
                                   {with {f {fun {y}
                                              {setv y 5}}}
                                     {seqn {f {ref v}}
                                           v}}})
                         (mtSub) (mtSto)))
      (numV 5))

(test (vxs-value (interp (parse '{with {swap {fun {x}
                                    {fun {y}
                                      {with {z x}
                                        {seqn {setv x y}
                                              {setv y z}}}}}}
                        {with {a 3}
                          {with {b 2}
                            {seqn {{swap {ref a}} {ref b}}
                              b}}}})
              (mtSub) (mtSto)))
      (numV 3))
