#lang racket

(require redex)

(provide (all-defined-out))

(define-language 
  lambdaPHP
  (scope ((x val) ...))
  (loc natural)
  (prim boolean number string null)
  (val prim (ref loc))
  (lbl x)
  (e val
     x
     (set e e)
     (begin e e ...)
     (label lbl e)
     (break lbl e))
  (H hole
     (set H e)
     (set val H)
     (begin val ... H e ...))
  (E hole
     (set E e)
     (set val E)
     (begin val ... E e ...)
     (label lbl E)
     (break lbl E))
  ((f g x y z) variable-not-otherwise-mentioned))

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
    (--> (((x_old val_old) ...)
          (in-hole E (set x_new val_new)))
         (((x_new val_new) (x_old val_old) ...)
          val_new)
         "E-Assign")
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
    [(--> (scope (in-hole E e_1)) (scope (in-hole E e_2)))
     (==> e_1 e_2)]))

(define-syntax trace
  (syntax-rules ()
    [(_ exp)
     (traces eval-lambdaPHP (term (() exp)))]))
