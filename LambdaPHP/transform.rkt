#lang racket

(require php-parser)
(require redex)

(provide (all-defined-out))

(define ex1 (php-parse (open-input-string "<?php
function factorial($number) { 
 
    if ($number < 2) { 
        return 1; 
    } else { 
        return ($number * factorial($number-1)); 
    } 
}
factorial(4);
?>")))


(define (parse p)
  (match p
    [(? cons?) (term (begin ,@(map parse p)))]
    [(? empty?) (term ())]
    [(? number?) p]
    
    [(Assign _ _ op l r _) 
     (match op
       ['ASSIGN `(set ,(parse l) ,(parse r))]
       [_ (error "unsupported op in Assign")])]
    [(Binary _ _ op l r _)
     (match op
       ['IS_IDENTICAL `(=== ,(parse l) ,(parse r))]
       ['MINUS `(- ,(parse l) ,(parse r))]
       ['MULT `(* ,(parse l) ,(parse r))]
       ['SMALLER `(< ,(parse l) ,(parse r))]
       [else (error op)])]
    [(BlockStmt _ _ s _) `(begin ,@(map parse s))]
    [(ExprStmt _ _ e _) (parse e)]
    [(FunctionCall _ _ e a _) `(,(parse e) ,@(map parse a))]
    [(FunctionCallParameter _ _ e _ _) (parse e)]
    [(FunctionDcl _ _ _ n a b _)
     (define (make-global n)
       (GlobalStmt null null (list (Variable null null n))))
     `(set ,(string->symbol n) (lambda (,@(map parse a))
                                 ,(append (parse (cons (make-global n) b)) 
                                          (list `(break 0ret null)))))]
    [(GlobalStmt _ _ l _) `(begin ,@(map (lambda (x) `(global ,(parse x))) l))]
    [(IfStmt _ _ c t _ e _) `(if (to-bool ,(parse c)) ,(parse t) ,(parse e))]
    [(NamespaceName _ _ _ n _) (string->symbol (first n))]
    [(ParameterDcl _ _ _ n _ _ _) (string->symbol n)]
    [(ReturnStmt _ _ e _) `(break 0ret ,(parse e))]
    [(Variable _ _ n _) (string->symbol n)]
    [_ (error p)]))
(parse ex1)