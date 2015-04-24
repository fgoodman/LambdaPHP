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
  (x-op op x)
  
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
  
  (H hole
     (op v ... H e ...)
     (c H)
     (v ... H e ...)
     (set! x H)
     (begin H e ...)
     (if H e e)
     (return H)
     (echo (v ... H e ...)))
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
  Lift : (begin e ...) -> (begin e ...)
  [(Lift (begin e ...))
   (begin ,@(call-with-values
             (λ () (partition (curry (redex-match? L (set! x_1 (λ (x ...) e))))
                              (term (e ...)))) append))])

(define gensym (let ([cnt 0])
                 (lambda () (begin (set! cnt (+ cnt 1))
                                   (string->symbol (format "tmp-~s" cnt))))))

(define-metafunction L
  ANF : e -> e
  [(ANF (x-op e ...))
   (begin ,@(let ([l (foldr (lambda (x b)
                              (let ([f (first b)]
                                    [r (second b)]
                                    [s (gensym)])
                                (if (redex-match? L (e_1 e_2 ...) x)
                                    (list (cons `(set! ,s ,(term (ANF ,x))) f)
                                          (cons s r))
                                    (list f (cons x r)))))
                            (list empty empty) (term (e ...)))])
              (append (first l) (list (cons (term x-op) (second l))))))]
  [(ANF (set! x e)) (set! x (ANF e))]
  [(ANF (echo (e ...))) (echo ((ANF e) ...))]
  [(ANF (any e ...)) (any (ANF e) ...) (side-condition (not (equal? (term any) (term λ))))]
  [(ANF e) e])

(define-metafunction L
  IF : P -> CF
  [(IF (<?php e)) ("" (()) () (ANF (Lift e)))])

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
   (--> (B (σ ... ((x_1 i_1) ...)) ((i_2 v_2) ...)
           (in-hole E ((λ (x ...) e) v ...)))
        (B (σ_n σ ... ((x_1 i_1) ...)) ((i_2 v_2) ...)
           (in-hole E (body (subst-n (x v) ... e))))
        E-β
        (where σ_n ,(foldr (lambda (a base)
                             (let ([v (assoc (second a)
                                             (term ((i_2 v_2) ...)))])
                               (if (and v (redex-match? L (λ (x_l ...) e_l)
                                                        (second v)))
                                   (cons a base) base)))
                           empty (term ((x_1 i_1) ...)))))
   
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
   (--> (B (σ_1 σ ... (name g ((x_2 i_2) ... (x i) (x_3 i_3) ...))) Σ
           (in-hole E (global x)))
        (B (((x i) . σ_1) σ ... g) Σ (in-hole E undef))
        E-Global)
   (--> (B (σ ... (name g ((x_1 i_1) ...))) Σ (in-hole E (global x)))
        (B (σ ... g) Σ (in-hole E undef))
        E-GlobalNull
        (side-condition (not (member (term x) (term (x_1 ...))))))
   
   ; Sequential statement
   (==> (begin v e_1 e_2 ...) (begin e_1 e_2 ...) E-Begin)
   (==> (begin v) v E-BeginFinal)
   
   ; Conditional statement
   (==> (if v e_1 e_2) (if (to-bool v) e_1 e_2) E-IfConv
        (side-condition (not (boolean? (term e_test)))))
   (==> (if #t e_1 e_2) e_1 E-IfTrue)
   (==> (if #f e_1 e_2) e_2 E-IfFalse)
   
   ; While statement
   (==> (while e_1 e_2) (if (to-bool e_1) (begin e_2 (while e_1 e_2)) undef)
        E-While)
   
   ; Return statement
   (--> (B (σ_1 σ ...) Σ (in-hole E (body (in-hole H (return v)))))
        (B (σ ...) Σ (in-hole E v))
        E-Return)
   
   ; Echo statement
   (==> (echo (v_f v_r ...)) (echo ((to-string v_f) v_r ...)) E-EchoValue
        (side-condition (not (string? (term v_f)))))
   (--> (B (σ ...) Σ (in-hole E (echo (v_f v_r ...))))
        (,(string-append (if (and (number? (term B))
                                  (= (floor (term B)) (term B)))
                             (inexact->exact (term B)) (term B))
                         (term v_f)) (σ ...) Σ (in-hole E (echo (v_r ...))))
        E-Echo
        (side-condition (string? (term v_f))))
   (==> (in-hole E (echo ())) (in-hole E undef) E-EchoFinal)
   
   with
   [(--> (B (σ ...) Σ (in-hole E e_1))
         (B (σ ...) Σ (in-hole E e_2)))
    (==> e_1 e_2)]))

;(render-reduction-relation L-reduce "rel.pdf")
