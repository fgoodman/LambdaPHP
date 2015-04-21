#lang racket

(require php-parser)

(provide desugar)

(define (desugar p)
  
  (define (make-Binary op)
    (lambda (lst) (Binary null null op (car lst) (cdr lst))))
  
  (define (make-Global n)
    (GlobalStmt null null (list (Variable null null n))))
  
  (match p
    [(or (? number?)
         (? null?)) p]
    
    ['BOOL_TRUE #t]
    ['BOOL_FALSE #f]
    [(? string?) (string-replace (string-replace p "\"" "") "\'" "")]
    
    [(? cons?) `(begin ,@(map desugar p) undef)]

    [(Assign _ _ op l r _)
     `(set ,(desugar l) ,(desugar ((match op
                                     ['PLUS_EQUAL (make-Binary 'PLUS)]
                                     ['MINUS_EQUAL (make-Binary 'MINUS)]
                                     ['MULT_EQUAL (make-Binary 'MULT)]
                                     ['DIV_EQUAL (make-Binary 'DIV)]
                                     ['MOD_EQUAL (make-Binary 'MOD)]
                                     ['CONCAT_EQUAL (make-Binary 'DOT)]
                                     [_ cdr]) (cons l r))))]
    
    [(Binary _ _ op l r _)
     `(,(match op
          ['PLUS `+]
          ['MINUS `-]
          ['MULT `*]
          ['DIV `/]
          ['MOD `%]
          
          ['DOT #\.]
          
          ['LOGICAL_OR `or]
          ['LOGICAL_AND `and]
          ['IS_IDENTICAL `===]
          ['IS_NOT_IDENTICAL `!==]
          ['IS_EQUAL `==]
          ['IS_NOT_EQUAL `!=]
          ['SMALLER `<]
          ['IS_SMALLER_OR_EQUAL `<=]
          ['GREATER `>]
          ['IS_GREATER_OR_EQUAL `>=]
          [_ (error (format "Binary operator ~s not found"
                            (pretty-format op)))])
       ,(desugar l) ,(desugar r))]
    
    [(BlockStmt _ _ s _) `(begin ,@(map desugar s) undef)]
    
    [(Cast _ _ t e _) `(,(match t
                           ['BOOL_CAST `to-bool]
                           ['INT_CAST `to-int]
                           ['DOUBLE_CAST `to-double]
                           ['STRING_CAST `to-string]) ,(desugar e))]
    
    [(EchoStmt _ _ s _) `(echo ,@(map desugar s))]
    
    [(ExprStmt _ _ e _) (desugar e)]
    
    [(ForLoop _ _ b t a s _)
     `(begin ,(desugar b) (while ,(desugar t) (begin ,(desugar s)
                                                     ,(desugar a)
                                                     undef)) undef)]
    
    [(FunctionCall _ _ e a _) `(,(desugar e) ,@(map desugar a))]
    
    [(FunctionCallParameter _ _ e _ _) (desugar e)]
    
    [(FunctionDcl _ _ _ n a b _)
     `(set ,(string->symbol n) (λ (,@(map desugar a))
                                 ,(append (desugar (cons (make-Global n) b))
                                          (list `(return null) `undef))))]
    
    [(GlobalStmt _ _ l _)
     `(begin ,@(map (lambda (x) `(global ,(desugar x))) l) undef)]
    
    [(IfStmt _ _ c t _ e _)
     `(if ,(desugar c) ,(desugar t) ,(desugar e))]
    
    #;[(Infix _ _ op e _)
     `(set ,(desugar e) (,(match op
                            ['INC `inc]
                            ['DEC `dec]) ,(desugar e)))]
    
    #;[(NamespaceName _ _ _ n _)
     (define s (string->symbol (first n)))
     (if (eq? s 'var_dump) `var-dump s)]
    
    [(ParameterDcl _ _ _ n _ _ _) (string->symbol n)]
    
    #;[(Postfix _ _ op e _)
     `(begin (set $$_tmp ,(desugar e))
             (set ,(desugar e) (,(match op
                                   ['INC `inc]
                                   ['DEC `dec]) ,(desugar e)))
             $$_tmp)]
    
    [(ReturnStmt _ _ e _) `(return ,(desugar e))]
    
    [(TestExpr _ _ c t e _)
     `(if ,(desugar c) ,(desugar t) ,(desugar e))]
    
    [(Unary _ _ op e _) `(,(match op
                             ['MINUS `-]
                             ['NEG `!]) ,(desugar e))]
    
    [(Variable _ _ n _) (string->symbol n)]
    
    [(WhileStmt _ _ c b _) `(while ,(desugar c) ,(desugar b))]
    
    [_ (error 
        (format "No pattern found for desugaring the following expression:\n~a"
                (pretty-format p)))]))