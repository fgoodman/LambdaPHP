#lang racket

(require redex)

(provide (all-defined-out))

(define (lambdaPHP-delta t)
  (define op (first t))
  (define ra (rest t))
  (match op
    ['to-bool (case (first ra)
                [(#f 0 0.0 "" "0" null) #f]
                [else #t])]))

(define-language 
  lambdaPHP
  (env ((x loc) ...))
  (envs (env ...))
  (sto ((loc val) ...))
  (loc natural)
  (prim (lambda (x ...) e) boolean number string null)
  (val prim)
  (lbl x)
  (op to-bool)
  (e val
     x
     (op e ...)
     (e e ...)
     (set e e)
     (if e e e)
     (begin e e ...)
     (label lbl e)
     (break lbl e))
  (H hole
     (op val ... E e ...)
     (val ... E e ...)
     (set val E)
     (if E e e)
     (begin E e ...)
     (break lbl E))
  (E H
     (label lbl E))
  (x variable-not-otherwise-mentioned))

(define-metafunction
  lambdaPHP
  delta : op val ... -> prim
  [(delta op val ...) ,(lambdaPHP-delta (term (op val ...)))])

(define-metafunction
  lambdaPHP
  subst-n : (x any) ... any -> any
  [(subst-n (x_1 any_1) (x_2 any_2) ... any_3)
   (subst x_1 any_1 (subst-n (x_2 any_2) ... any_3))]
  [(subst-n any_3) any_3])

(define-metafunction
  lambdaPHP
  subst : x any any -> any
  [(subst x_1 any_1 (label any_2 e_2))
   (label any_2 (subst x_1 any_1 e_2))]
   [(subst x_1 any_1 (break any_2 e_2))
   (break any_2 (subst x_1 any_1 e_2))]
  [(subst x_1 any_1 (lambda (x_2 ... x_1 x_3 ...) any_2))
   (lambda (x_2 ... x_1 x_3 ...) any_2)
   (side-condition (not (member (term x_1) (term (x_2 ...)))))]
  [(subst x_1 any_1 (lambda (x_2 ...) any_2))
   ,(term-let ([(x_new ...)
                (variables-not-in
                 (term (x_1 any_1 any_2))
                 (term (x_2 ...)))])
              (term
               (lambda (x_new ...)
                 (subst x_1 any_1
                        (subst-vars (x_2 x_new) ...
                                    any_2)))))]
  [(subst x_1 any_1 x_1) any_1]
  [(subst x_1 any_1 x_2) x_2]
  [(subst x_1 any_1 (any_2 ...))
   ((subst x_1 any_1 any_2) ...)]
  [(subst x_1 any_1 any_2) any_2])

(define-metafunction
  lambdaPHP
  subst-vars : (x any) ... any -> any
  [(subst-vars (x_1 any_1) ... (label x_2 any_2))
   (label x_2 (subst-vars (x_1 any_1) ... any_2))]
  [(subst-vars (x_1 any_1) ... (break x_2 any_2))
   (break x_2 (subst-vars (x_1 any_1) ... any_2))]
  [(subst-vars (x_1 any_1) x_1) any_1]
  [(subst-vars (x_1 any_1) (any_2 ...))
   ((subst-vars (x_1 any_1) any_2) ...)]
  [(subst-vars (x_1 any_1) any_2) any_2]
  [(subst-vars (x_1 any_1) (x_2 any_2) ... any_3)
   (subst-vars (x_1 any_1)
               (subst-vars (x_2 any_2) ... any_3))]
  [(subst-vars any) any])

(define genloc (let ([cnt 0]) (lambda () (begin (set! cnt (+ cnt 1)) cnt))))

(define eval-lambdaPHP
  (reduction-relation
    lambdaPHP
    (--> ((((x_1 loc_1) ... (x_cur loc) (x_2 loc_2) ...) env ...)
          ((loc_3 val_3) ... (loc val_old) (loc_4 val_4) ...)
          (in-hole E (set x_cur val_new)))
         ((((x_1 loc_1) ... (x_cur loc) (x_2 loc_2) ...) env ...)
          ((loc_3 val_3) ... (loc val_new) (loc_4 val_4) ...)
          (in-hole E val_new))
         "E-Alloc+Assign")
    (--> ((((x_1 loc_1) ...) env ...)
          ((loc_2 val_2) ...)
          (in-hole E (set x_new val_new)))
         ((((x_new loc_new) (x_1 loc_1) ...) env ...)
          ((loc_new val_new) (loc_2 val_2) ...)
          (in-hole E val_new))
         "E-Assign"
         (where loc_new ,(genloc))
         (side-condition (not (memq (term x_new) (term (x_1 ...))))))
    (--> ((env ...)
          sto
          (in-hole E ((lambda (x ...) e) val ...)))
         ((() env ...)
          sto
          (in-hole E (label 0ret (subst-n (x val) ... e))))
         "E-Beta")
    (--> ((((x_1 loc_1) ... (x loc) (x_2 loc_2) ...) env ...)
          ((loc_3 val_3) ... (loc val) (loc_4 val_4) ...)
          (in-hole E x))
         ((((x_1 loc_1) ... (x loc) (x_2 loc_2) ...) env ...)
          ((loc_3 val_3) ... (loc val) (loc_4 val_4) ...)
          (in-hole E val))
         "E-Subst")
    (--> ((((x_1 loc_1) ...) env ...)
          sto
          (in-hole E x))
         ((((x_1 loc_1) ...) env ...)
          sto
          (in-hole E null))
         "E-SubstNull"
         (side-condition (not (memq (term x) (term (x_1 ...))))))
    (==> (op val ...) (delta op val ...)
         "E-Prim")
    (==> (if #t e_1 e_2)
         e_1
         "E-IfTrue")
    (==> (if #f e_1 e_2)
         e_2
         "E-IfFalse")
    (==> (begin val e_1 e_2 ...)
         (begin e_1 e_2 ...)
         "E-Begin")
    (==> (begin val)
         val
         "E-BeginFinal")
    (==> (label lbl (in-hole H (break lbl val)))
         val
         "E-Label-Match"
         (side-condition (not (eq? (term lbl) (term 0ret)))))
    (--> ((env_local env ...)
          sto
          (in-hole E (label lbl (in-hole H (break lbl val)))))
         ((env ...)
          sto
          (in-hole E val))
         "E-Label-Return"
         (side-condition (eq? (term lbl) (term 0ret))))
    (==> (label lbl_1 (in-hole H (break lbl_2 val)))
         (break lbl_2 val)
         "E-Label-Pop"
         (side-condition (not (equal? (term lbl_1) (term lbl_2)))))
    (==> (break lbl_1 (in-hole H (break lbl_2 val)))
         (break lbl_2 val)
         "E-Break-Break")
    (==> (label lbl_1 val)
         val
         "E-Label-Pop-NoBreak")
    with
    [(--> (envs sto (in-hole E e_1)) (envs sto (in-hole E e_2)))
     (==> e_1 e_2)]))

(define-syntax trace
  (syntax-rules ()
    [(_ exp)
     (traces eval-lambdaPHP (term ((()) () exp)))]))