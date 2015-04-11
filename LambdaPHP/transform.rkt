#lang racket

(require php-parser)
(require redex)

(provide (all-defined-out))

(define ex1 (php-parse (open-input-string "<?php
$x = 0;
while ($x < 3) {
$x += 1;
}
?>")))


(define (desugar p)
  (define (make-Binary op)
    (lambda (lst) (Binary null null op (car lst) (cdr lst))))
  (define (make-Global n)
    (GlobalStmt null null (list (Variable null null n))))
  
  (match p
    [(? cons?) (term (begin ,@(map desugar p)))]
    [(? empty?) (term ())]
    [(? number?) p]
    
    [(Assign _ _ op l r _)
     `(set ,(desugar l) ,(desugar ((match op ; Bitwise assignment operators
                                             ; are excluded
                                     ['PLUS_EQUAL (make-Binary 'PLUS)]
                                     ['MINUS_EQUAL (make-Binary 'MINUS)]
                                     ['MULT_EQUAL (make-Binary 'MULT)]
                                     ['EXPO_EQUAL (make-Binary 'EXPO)]
                                     ['DIV_EQUAL (make-Binary 'DIV)]
                                     ['MOD_EQUAL (make-Binary 'MOD)]
                                     ['AND_EQUAL (make-Binary 'LOGICAL_AND)]
                                     ['OR_EQUAL (make-Binary 'LOGICAL_OR)]
                                     ['XOR_EQUAL (make-Binary 'LOGICAL_XOR)]
                                     ['CONCAT_EQUAL (make-Binary 'DOT)]
                                     [else cdr]) (cons l r))))]
    
    [(Binary _ _ op l r _)
     `(,(match op
          ; Bitwise operators are excluded
          
          ; Arithmetic operators
          ['PLUS `+]
          ['MINUS `-]
          ['MULT `*]
          ['EXPO `**]
          ['DIV `/]
          ['MOD `%]
          ['DOT #\.]
          
          ; Logical opeartors
          ['BOOLEAN_OR (error op)]  ;
          ['BOOLEAN_AND (error op)] ; Not sure what either of thse do...
          ['LOGICAL_OR `or]
          ['LOGICAL_AND `and]
          ['LOGICAL_XOR 'xor]
          ['IS_IDENTICAL `===]
          ['IS_NOT_IDENTICAL `!==]
          ['IS_EQUAL `==]
          ['IS_NOT_EQUAL `!=]
          ['SMALLER `<]
          ['IS_SMALLER_OR_EQUAL `<=]
          ['GREATER `>]
          ['IS_GREATER_OR_EQUAL `>=])
       ,(desugar l) ,(desugar r))]
    
    [(BlockStmt _ _ s _) `(begin ,@(map desugar s))]
    
    [(ExprStmt _ _ e _) (desugar e)]
    
    [(FunctionCall _ _ e a _) `(,(desugar e) ,@(map desugar a))]
    
    [(FunctionCallParameter _ _ e _ _) (desugar e)]
    
    [(FunctionDcl _ _ _ n a b _)
     `(set ,(string->symbol n) (lambda (,@(map desugar a))
                                 ; Put function name in scope for recursion
                                 ,(append (desugar (cons (make-Global n) b))
                                          ; All functions must return something
                                          (list `(break $ret null)))))]
    
    [(GlobalStmt _ _ l _)
     `(begin ,@(map (lambda (x) `(global ,(desugar x))) l))]
    
    [(IfStmt _ _ c t _ e _)
     `(if (to-bool ,(desugar c)) ,(desugar t) ,(desugar e))]
    
    [(Infix _ _ op e _)
     `(set ,(desugar e) ,(desugar ((make-Binary (match op
                                                  ['INC 'PLUS]
                                                  ['DEC 'MINUS])) (cons e 1))))]
    
    [(NamespaceName _ _ _ n _) (string->symbol (first n))]
    
    [(ParameterDcl _ _ _ n _ _ _) (string->symbol n)]
    
    [(Postfix _ _ op e _)
     (desugar ((make-Binary (match op
                              ['INC 'MINUS]
                              ['DEC 'PLUS]))
               (cons (Infix null null op e) 1)))]
    
    [(ReturnStmt _ _ e _) `(break $ret ,(desugar e))]
    
    [(TestExpr _ _ c t e _)
     `(if (to-bool ,(desugar c)) ,(desugar t) ,(desugar e))]
    
    [(Variable _ _ n _) (string->symbol n)]
    
    [(WhileStmt _ _ c b _) `(while ,(desugar c) ,(desugar b))]
    
    [_ (begin (print p) (error "Unmatched case"))]))
(desugar ex1)