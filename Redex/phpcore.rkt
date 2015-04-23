#lang racket

(require redex
         "phpdelta.rkt")

(provide (all-defined-out))

(define-language L
  (P (<?php e))
  
  (v (λ (x ...) e) boolean number string null undef)
  
  (c to-bool to-int to-double to-string
     to-number)
  (op + - * / %
      #\.
      or and === !== == != < <= > >=
      !)
  (c-op op c)
  
  (e v
     x
     (op e ...)
     (c e)
     (e e ...)
     (set! x e)
     
     (global x)
     (begin e e ...)
     (if e e e)
     (while e e)
     (return e)
     (body e)
     (echo (e ...)))
  
  (E hole
     (op v ... E e ...)
     (c E)
     (v ... E e ...)
     (set! x E)
     (begin E e ...)
     (if E e e)
     (return E)
     (body E)
     (echo (v ... E e ...)))
  
  (x variable-not-otherwise-mentioned)
  
  (CF (B (σ σ ...) Σ e))
  (B string)
  (σ ((x i) ...))
  (Σ ((i v) ...))
  (i natural))

(define-metafunction L
  IF : P -> CF
  [(IF (<?php e)) ("" (()) () e)])

(define-metafunction L
  subst-n : (x any) ... any -> any
  [(subst-n (x_1 any_1) (x_2 any_2) ... any_3)
   (subst x_1 any_1 (subst-n (x_2 any_2) ... any_3))]
  [(subst-n any_3) any_3])

(define-metafunction L
  subst : x any any -> any
  [(subst x_1 any_1 (λ (x_2 ... x_1 x_3 ...) any_2))
   (λ (x_2 ... x_1 x_3 ...) any_2)
   (side-condition (not (member (term x_1) (term (x_2 ...)))))]
  [(subst x_1 any_1 (λ (x_2 ...) any_2))
   ,(term-let ([(x_new ...) (variables-not-in (term (x_1 any_1 any_2))
                                              (term (x_2 ...)))])
              (term (λ (x_new ...)
                      (subst x_1 any_1 (subst-vars (x_2 x_new) ... any_2)))))]
  [(subst x_1 any_1 x_1) any_1]
  [(subst x_1 any_1 x_2) x_2]
  [(subst x_1 any_1 (any_2 ...)) ((subst x_1 any_1 any_2) ...)]
  [(subst x_1 any_1 any_2) any_2])

(define-metafunction L
  subst-vars : (x any) ... any -> any
  [(subst-vars (x_1 any_1) x_1) any_1]
  [(subst-vars (x_1 any_1) (any_2 ...))
   ((subst-vars (x_1 any_1) any_2) ...)]
  [(subst-vars (x_1 any_1) any_2) any_2]
  [(subst-vars (x_1 any_1) (x_2 any_2) ... any_3)
   (subst-vars (x_1 any_1)
               (subst-vars (x_2 any_2) ... any_3))]
  [(subst-vars any) any])

(define-metafunction L
  δ : c-op v ... -> e
  [(δ c-op v ...) ,(δ-apply (term (c-op v ...)))])

(define L-reduce
  (reduction-relation
   L #:domain CF
   
   ; Variable substitution
   (--> (B (((x_1 i_1) ... (x i) (x_2 i_2) ...) σ ...)
             ((i_3 v_3) ... (i v) (i_4 v_4) ...)
             (in-hole E x))
        (B (((x_1 i_1) ... (x i) (x_2 i_2) ...) σ ...)
             ((i_3 v_3) ... (i v) (i_4 v_4) ...)
             (in-hole E v))
        E-Subst)
   (--> (B (((x_1 i_1) ...) σ ...) Σ (in-hole E x))
        (B (((x_1 i_1) ...) σ ...) Σ (in-hole E null))
        E-SubstNull
        (side-condition (not (member (term x) (term (x_1 ...))))))
   
   ; Primitive application
   (==> (op v ...) (δ op v ...) E-Prim)
   
   ; Casting
   (==> (c v) (δ c v) E-Cast)
   
   ; Function application
   (--> (B (σ ...) Σ (in-hole E ((λ (x ...) e) v ...)))
        (B (() σ ...) Σ (in-hole E (body (subst-n (x v) ... e))))
        E-β)
   
   ; Variable assignment
   (--> (B (((x_1 i_1) ... (x_new i_cur) (x_2 i_2) ...) σ ...)
             ((i_3 v_3) ... (i_cur v_cur) (i_4 v_4) ...)
             (in-hole E (set! x_new v_new)))
        (B (((x_1 i_1) ... (x_new i_cur) (x_2 i_2) ...) σ ...)
             ((i_3 v_3) ... (i_cur v_new) (i_4 v_4) ...)
             (in-hole E v_new))
        E-Update)
   (--> (B (((x_1 i_1) ...) σ ...) () (in-hole E (set! x_new v_new)))
        (B (((x_new 0) (x_1 i_1) ...) σ ...) ((0 v_new)) (in-hole E v_new))
        E-AssignFirst
        (side-condition (not (member (term x_new) (term (x_1 ...))))))
   (--> (B (((x_1 i_1) ...) σ ...)
             ((i_2 v_2) (i_r v_r) ...)
             (in-hole E (set! x_new v_new)))
        (B (((x_new i_new) (x_1 i_1) ...) σ ...)
             ((i_new v_new) . ((i_2 v_2) (i_r v_r) ...))
             (in-hole E v_new))
        E-Assign
        (where i_new ,(add1 (term i_2)))
        (side-condition (not (member (term x_new) (term (x_1 ...))))))
         
   ; Global statement
   (--> (B (((x_1 i_1) ...) σ ... ((x_2 i_2) ... (x i) (x_3 i_3) ...))
             Σ
             (in-hole E (global x)))
        (B (((x_1 i_1) ...) σ ... ((x_2 i_2) ... (x i) (x_3 i_3) ...))
             Σ
             (in-hole E undef))
        E-Global)
   
   ; Sequential statement
   (==> (begin v e_1 e_2 ...) (begin e_1 e_2 ...) E-Begin)
   (==> (begin v) v E-BeginFinal)
   
   ; Conditional statement
   (==> (if v e_1 e_2) (if (to-bool v) e_1 e_2) E-IfConv
        (side-condition (not (boolean? (term e_test)))))
   (==> (if #t e_1 e_2) e_1 E-IfTrue)
   (==> (if #f e_1 e_2) e_2 E-IfFalse)
   
   ; While statement
   (==> (while e_1 e_2) (if (to-bool e_1) (begin e_2 (while e_1 e_2) undef))
        E-While)
   
   ; Return statement
   (--> (B (σ_1 σ ...) Σ (in-hole E (body (return v))))
        (B (σ ...) Σ (in-hole E v))
        E-Return)
   
   ; Echo statement
   (--> (B (σ ...) Σ (in-hole E (echo (v_f v_r ...))))
        (B (σ ...) Σ (in-hole E (echo ((to-string v_f) v_r ...))))
        E-EchoValue
        (side-condition (not (string? (term v_f)))))
   (--> (B (σ ...) Σ (in-hole E (echo (v_f v_r ...))))
        (,(string-append (term B)
                         (term v_f)) (σ ...) Σ (in-hole E (echo (v_r ...))))
        E-Echo
        (side-condition (string? (term v_f))))
   (==> (in-hole E (echo ())) (in-hole E undef) E-EchoFinal)
   
   with
   [(--> (B (σ ...) Σ (in-hole E e_1))
         (B (σ ...) Σ (in-hole E e_2)))
    (==> e_1 e_2)]))

;(render-reduction-relation L-reduce "rel.pdf")
