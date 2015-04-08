#lang racket

(require redex)

(provide (all-defined-out))

(define-language 
  lambdaPHP
  (sto ((loc val) ...))
  (loc natural)
  (prim boolean number string null)
  (val prim (ref loc))
  (lbl x)
  (e val
     x
     (let ([x e] ...) e)
     (set! e e)
     (alloc e)
     (deref e)
     (begin e e ...)
     (label lbl e)
     (break lbl e))
  (H hole
     (let ([x val] ... [y H] [z e] ...) e)
     (set! H e)
     (set! val H)
     (alloc H)
     (deref H)
     (begin val ... H e ...))
  (E hole
     (let ([x val] ... [y E] [z e] ...) e)
     (set! E e)
     (set! val E)
     (alloc E)
     (deref E)
     (begin val ... E e ...)
     (label lbl E)
     (break lbl E))
  ((f g x y z) variable-not-otherwise-mentioned))

(define-metafunction 
  lambdaPHP
  alloc-def : val sto -> (loc sto)
  [(alloc-def val_n ((loc val) ...))
   ,(term-let ([loc_n (+ 1 (apply max (term (0 loc ...))))])
              (term (loc_n ((loc_n val_n) (loc val) ...))))])

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
  [(subst y any_1 (let ([x_2 e_2] ...
                        [y e_y]
                        [x_3 e_3] ...)
                    e_4))
   (let ([x_2 (subst y any_1 e_2)] ...
         [y (subst y any_1 e_y)]
         [x_3 (subst y any_1 e_3)] ...)
     e_4)
   (side-condition (not (member (term x_1) (term (x_2 ...)))))]
  [(subst x_1 any_1 (let ([x_2 e_2] ...) e_3))
   ,(term-let ([(x_new ...) (variables-not-in (term (x_1 any_1 e_3))
                                              (term (x_2 ...)))])
              (term (let ([x_new (subst x_1 any_1 e_2)] ...)
                      (subst x_1 any_1 (subst-vars (x_2 x_new) ... e_3)))))]
  [(subst x_1 any_1 x_1) any_1]
  [(subst x_1 any_1 x_2) x_2]
  [(subst x_1 any_1 any_2) any_2])

(define-metafunction
  lambdaPHP
  subst-vars : (x any) ... any -> any
  [(subst-vars (x_1 any_1) ... (label x_2 any_2))
   (label x_2 (subst-vars (x_1 any_1) ... any_2))]
  [(subst-vars (x_1 any_1) ... (break x_2 any_2))
   (break x_2 (subst-vars (x_1 any_1) ... any_2))]
  [(subst-vars (x_1 any_1) x_1) any_1]
  [(subst-vars (x_1 any_1) any_2) any_2]
  [(subst-vars (x_1 any_1) (x_2 any_2) ... any_3)
   (subst-vars (x_1 any_1) (subst-vars (x_2 any_2) ... any_3))]
  [(subst-vars any) any])

(define eval-lambdaPHP
  (reduction-relation
    lambdaPHP
    (--> (((loc_1 val_1) ... (loc val_old) (loc_3 val_3) ...)
          (in-hole E (set! (ref loc) val_new)))
         (((loc_1 val_1) ... (loc val_new) (loc_3 val_3) ...)
          (in-hole E (ref loc)))
         "E-Assign")
    (==> (let ([x val] ...) e)
         (subst-n (x val) ... e)
         "E-Let")
    (--> (sto_1 (in-hole E (alloc val)))
         (sto_2 (in-hole E (ref loc)))
         "E-Alloc"
         (where (loc sto_2) (alloc-def val sto_1)))
    (--> (((loc_1 val_1) ... (loc_2 val_2) (loc_3 val_3) ...)
          (in-hole E (deref (ref loc_2))))
         (((loc_1 val_1) ... (loc_2 val_2) (loc_3 val_3) ...)
          (in-hole E val_2))
         "E-Deref")
    (==> (begin val ... val_r)
         val_r
         "E-BeginResult")
    (==> (label lbl (in-hole H (break lbl val)))
         val
         "E-Label-Match")
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
    [(--> (sto (in-hole E e_1)) (sto (in-hole E e_2)))
     (==> e_1 e_2)]))

(define-syntax trace
  (syntax-rules ()
    [(_ exp)
     (traces eval-lambdaPHP (term (() exp)))]))
