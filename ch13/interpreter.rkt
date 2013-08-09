#lang plai

;; define BCFAE, BCFAE-Value, Env, Store, and ValuexStore
(define-type BCFAE
  [num (n number?)]
  [add (lhs BCFAE?) (rhs BCFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body BCFAE?)]
  [app (fun-expr BCFAE?) (arg-expr BCFAE?)]
  [if0 (cond-expr BCFAE?) (succ-expr BCFAE?) (fail-expr BCFAE?)]
  [newbox (box-val BCFAE?)]
  [setbox (box-expr BCFAE?) (value-exr BCFAE?)]
  [openbox (box-expr BCFAE?)]
  [seqn (sequent1 BCFAE?) (sequent2 BCFAE?)])

(define-type BCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body BCFAE?)
            (env Env?)]
  [boxV (location number?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?)
        (location number?)
        (env Env?)])

(define-type Store
  [mtSto]
  [aSto (location number?)
        (value BCFAE-Value?)
        (store Store?)])

(define-type ValuexStore
  [vxs (value BCFAE-Value?) (store Store?)])

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
        [(newbox) (newbox (parse (second sexp)))]
        [(setbox) (setbox (parse (second sexp))
                          (parse (third sexp)))]
        [(openbox) (openbox (parse (second sexp)))]
        [(seqn) (seqn (parse (second sexp))
                      (parse (third sexp)))]
        [else [app (parse (first sexp)) (parse (second sexp))]])]))

;; interp : BCFAE Env -> ValuexStore
(define (interp expr env store)
  (type-case BCFAE expr
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
    [newbox (value-expr)
            (type-case ValuexStore (interp value-expr env store)
              [vxs (expr-value expr-store)
                (local ([define new-loc (next-location expr-store)])
                  (vxs (boxV new-loc)
                       (aSto new-loc expr-value expr-store)))])]
    [setbox (box-expr value-expr)
            (type-case ValuexStore (interp box-expr env store)
              [vxs (box-value box-store)
                (type-case ValuexStore (interp value-expr env box-store)
                  [vxs (value-value value-store)
                    (vxs value-value
                         (aSto (boxV-location box-value)
                               value-value
                               value-store))])])]
    [openbox (box-expr)
             (type-case ValuexStore (interp box-expr env store)
               [vxs (box-value box-store)
                 (vxs (store-lookup (boxV-location box-value)
                                     box-store)
                      box-store)])]
    [seqn (e1 e2)
          (type-case ValuexStore (interp e1 env store)
            [vxs (e1-value e1-store)
              (interp e2 env e1-store)])]
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

;; num-zero? : BCFAE-Value -> boolean
(define (num-zero? n) (zero? (numV-n n)))

;; env-lookup : symbol Env -> location
(define (env-lookup name env)
  (type-case Env env
    [mtSub () (error 'env-lookup "no binding for identifier ~s" name)]
    [aSub (bound-name bound-location rest-env)
          (if (symbol=? bound-name name)
              bound-location
              (env-lookup name rest-env))]))

;; store-lookup : location Store -> BCFAE-Value
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

;;
(test (interp (parse '{newbox 0}) (mtSub) (mtSto))
      (vxs (boxV 0) (aSto 0 (numV 0) (mtSto))))

(test/exn (interp (parse '{with {a {newbox 1}}
                            {seqn {with {b 3}
                                    b}
                              b}}) (mtSub) (mtSto))
          "env-lookup: no binding for identifier")

(test (vxs-value (interp (parse '{with {b {newbox 0}}
                                   {setbox b 1}})
                         (mtSub) (mtSto)))
      (numV 1))

(test (vxs-value (interp (parse '{with {b {newbox 0}}
                                   {seqn {setbox b {+ 1 {openbox b}}}
                                         {openbox b}}})
                         (mtSub) (mtSto)))
       (numV 1))

(test (vxs-value (interp (parse '{with {a {newbox 1}}
                                   {with {f {fun {x} {+ x {openbox a}}}}
                                     {seqn
                                       {setbox a 2}
                                       {f 5}}}})
                         (mtSub) (mtSto)))
           (numV 7))

(test (vxs-value (interp (parse '{with {x 3}
                                   {with {f {fun {y} {+ x y}}}
                                     {with {x 5}
                                       {f 10}}}})
                         (mtSub) (mtSto)))
      (numV 13))

(test (vxs-value (interp (parse '{with {b {newbox 0}}
                                   {if0 {seqn {setbox b 5}
                                              {openbox b}}
                                        1
                                        {openbox b}}})
                          (mtSub) (mtSto)))
      (numV 5))

(test (vxs-value (interp (parse '{with {switch {newbox 0}}
                                   {with {toggle {fun {dum}
                                                     {if0 {openbox switch}
                                                         {seqn
                                                           {setbox switch 1}
                                                           1}
                                                         {seqn
                                                           {setbox switch 0}
                                                           0}}}}
                                     {+ {toggle 1729}
                                        {toggle 1729}}}})
                         (mtSub) (mtSto)))
      (numV 1))
