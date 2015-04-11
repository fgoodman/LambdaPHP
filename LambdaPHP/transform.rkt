#lang racket

(require php-parser)
(require redex)

(provide (all-defined-out))

(define ex1 (php-parse (open-input-string "<?php

?>")))


(define (parse p)
  (match p
    [(? list?) (term (begin ,@(map parse p)))]
    [(? number?) p]
    
    [(Assign _ _ op l r _) 
     (match op
       ['ASSIGN `(set ,(parse l) ,(parse r))]
       [_ (error "unsupported op in Assign")])]
    [(BlockStmt _ _ s _) `(begin ,@(map parse s))]
    [(ExprStmt _ _ e _) (parse e)]
    [(FunctionCall _ _ e a _) `(,(parse e) ,@(map parse a))]
    [(FunctionCallParameter _ _ e _ _) (parse e)]
    [(FunctionDcl _ _ _ n a b _) `(set ,(string->symbol n) (lambda (,@(map parse a))
                                            ,(append (parse b) (list `(break 0ret null)))))]
    [(IfStmt _ _ c t _ e _) `(if (to-bool ,(parse c)) ,(parse t) ,(parse e))]
    [(NamespaceName _ _ _ n _) (string->symbol (first n))]
    [(ParameterDcl _ _ _ n _ _ _) (string->symbol n)]
    [(ReturnStmt _ _ e _) `(break 0ret ,(parse e))]
    [(Variable _ _ n _) (string->symbol n)]
    [_ (error "unsupported expression or statement in parse")]))
;ex1
(parse ex1)