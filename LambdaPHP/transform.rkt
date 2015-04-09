#lang racket

(require php-parser)
(require redex)

(provide (all-defined-out))

(define ex1 (php-parse (open-input-string "<?php

function foo($bar) {
$x = 1;
$y = 2;
}

?>")))


(define (parse p)
  (match p
    [(? list?) (term (begin ,@(map parse p)))]
    [(? number?) p]
    
    [(Assign _ _ op l r _) 
     (match op
       ['ASSIGN `(set ,(parse l) ,(parse r))]
       [_ (error "unsupported op in Assign")])]
    [(ExprStmt _ _ e _) (parse e)]
    [(FunctionDcl _ _ _ n a b _) `(set ,n (lambda () ,(append (parse b) (list null))))]
    [(ParameterDcl _ _ _ n _ _ _) (string->symbol n)]
    [(Variable _ _ n _) (string->symbol n)]
    [_ (error "unsupported expression or statement in parse")]))
;ex1
(parse ex1)