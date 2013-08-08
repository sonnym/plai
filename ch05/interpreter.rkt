#lang plai

;; define F1WAE, FunDef, and DefrdSub
(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?) (rhs F1WAE?)]
  [with (name symbol?) (named-expr F1WAE?) (body F1WAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?) (arg F1WAE?)])

(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body F1WAE?)])

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value number?) (ds DefrdSub?)])

;; parse : sexp -> F1WAE
;; to convert s-expressions into F1WAEs
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
        [(with) (with (first (second sexp))
                      (parse (second (second sexp)))
                      (parse (third sexp)))]
        [else (app (first sexp) (parse (second sexp)))])]))

;; interp : F1WAE listof(fundef) DefrdSub -> number
(define (interp expr fun-defs ds)
  (type-case F1WAE expr
    [num (n) n]
    [add (l r) (+ (interp l fun-defs ds) (interp r fun-defs ds))]
    [with (bound-id named-expr bound-body)
          (interp bound-body 
                  fun-defs
                  (aSub bound-id
                        (interp named-expr
                                fun-defs
                                ds)
                        ds))]
    [id (v) (lookup v ds)]
    [app (fun-name arg-expr)
         (local ([define the-fun-def (lookup-fundef fun-name fun-defs)])
           (interp (fundef-body the-fun-def)
                   fun-defs
                   (aSub (fundef-arg-name the-fun-def)
                         (interp arg-expr fun-defs ds)
                         (mtSub))))]))

;; lookup-fundef : symbol listof(FunDef) -> FunDef
(define (lookup-fundef fun-name fundefs)
  (cond
    [(empty? fundefs) (error fun-name "function not found")]
    [else (if (symbol=? fun-name (fundef-fun-name (first fundefs)))
      (first fundefs)
      (lookup-fundef fun-name (rest fundefs)))]))

;; lookup : symbol DefrdSub -> F1WAE
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-ds)
      (if (symbol=? bound-name name)
          bound-value
          (lookup name rest-ds))]))

;; tests repurposed from ch3
(test (interp (parse '5) '() (mtSub)) 5)
(test (interp (parse '{+ 5 5}) '() (mtSub)) 10)
(test (interp (parse '{with {x {+ 5 5}} {+ x x}}) '() (mtSub)) 20)
(test (interp (parse '{with {x 5} {+ x x}}) '() (mtSub)) 10)
(test (interp (parse '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}) '() (mtSub)) 14)
(test (interp (parse '{with {x 5} {with {y {- x 3}} { + y y}}}) '() (mtSub)) 4)
(test (interp (parse '{with {x 5} {+ x {with {x 3} 10}}}) '() (mtSub)) 15)
(test (interp (parse '{with {x 5} {+ x {with {x 3} x}}}) '() (mtSub)) 8)
(test (interp (parse '{with {x 5} {+ x {with {y 3} x}}}) '() (mtSub)) 10)
(test (interp (parse '{with {x 5} {with {y x} y}}) '() (mtSub)) 5)
(test (interp (parse '{with {x 5} {with {x x} x}}) '() (mtSub)) 5)

;; tests repurposed from ch4
(test (interp (parse '{double {double 5}})
              (list (fundef 'double
                            'n
                            (add (id 'n) (id 'n))))
              (mtSub))
      20)
(test (interp (parse '{add-1 {add-2 1}})
              (list (fundef 'add-2
                            'n
                            (add (app 'add-1 (num 1)) (id 'n)))
                    (fundef 'add-1
                            'n
                            (add (num 1) (id 'n))))
              (mtSub))
      4)
(test (interp (parse '{quadruple 5})
              (list (fundef 'double
                            'n
                            (add (id 'n) (id 'n)))
                    (fundef 'quadruple
                            'n
                            (add (app 'double (id 'n)) (app 'double (id 'n)))))
              (mtSub))
      20)

;;
(test/exn (interp (parse '{with {n 5} {f 10}})
                  (list (fundef 'f 'p (id 'n)))
                  (mtSub))
          "lookup: no binding for identifier")
