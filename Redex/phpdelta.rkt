#lang racket

(provide δ-apply)

; http://perldoc.perl.org/perlretut.html#Non-capturing-groupings
(define (is-numerical str)
  (cons? (regexp-match
          (pregexp
           "[+-]? *(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eE][+-]?\\d+)?") str)))

(define (strtod str) ; close enough...
  (define m (regexp-match
             (pregexp
              "[+-]? *(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eE][+-]?\\d+)?") str))
  (if (cons? m) (string->number (string-trim (first m))) 0))

; http://php.net/manual/en/language.types.boolean.php#language.types.boolean.casting
(define (to-bool v)
  (case v
    [(#f 0 0.0 "" "0" null) #f]
    [else #t]))

; http://php.net/manual/en/language.types.string.php#language.types.string.casting
(define (to-string v)
  (match v
    [#t "1"]
    [#f ""]
    [(? null?) ""]
    [(? number?) (number->string v)]
    [(? string?) v]))

; http://php.net/manual/en/language.types.integer.php#language.types.integer.casting
(define (to-int v)
   (match v
     [#t 1]
     [#f 0]
     [null 0]
     [(? number?) (floor v)]
     [(? string?) (floor (strtod v))]))

; http://php.net/manual/en/language.types.float.php#language.types.float.casting
(define (to-double v)
   (match v
     [#t 1.0]
     [#f 0.0]
     [null 0.0]
     [(? number?) (* 1.0 v)]
     [(? string?) (* 1.0 (strtod v))]))

(define (to-number v)
  (match v
    [(? string?) (strtod v)]
    [(? number?) v]
    [else (to-int v)]))

; http://php.net/manual/en/language.operators.comparison.php
(define (cmp op v_1 v_2)
  (define sp (match op
               [< string<?]
               [<= string<=?]
               [> string>?]
               [>= string>=?]
               [else string=?]))
  (cond
    [(and (or (null? v_1) (string? v_1)) (string? v_2))
     (if (or (is-numerical v_1) (is-numerical v_2))
         `(,op (to-number ,(if (null? v_1) `(to-string ,v_1) v_1))
               (to-number ,v_2))
         (if (and (string? v_1) (string? v_2))
             (sp v_1 v_2)
             `(,op ,(if (null? v_1) `(to-string ,v_1) v_1) v_2)))]
    [(and (string? v_1) (or (null? v_2) (string? v_2)))
     (if (or (is-numerical v_1) (is-numerical v_2))
         `(,op (to-number ,v_1)
               (to-number ,(if (null? v_2) `(to-string ,v_2) v_2)))
         (if (and (string? v_1) (string? v_2))
             (sp v_1 v_2)
             `(,op ,v_1 ,(if (null? v_2) `(to-string ,v_2) v_2))))]
    [(and (boolean? v_1) (null? v_1)) `(,op ,v_1 (to-bool ,v_2))]
    [(and (boolean? v_2) (null? v_2)) `(,op (to-bool ,v_1) ,v_2)]
    [else (op (to-number v_1) (to-number v_2))]))

(define δ-apply
  (match-lambda
    [`(to-bool ,v) (to-bool v)]
    [`(to-int ,v) (to-int v)]
    [`(to-double ,v) (to-double v)]
    [`(to-string ,v) (to-string v)]
    
    [`(to-number ,v) (to-number v)]
    
    [`(+ ,(? number? v_1) ,(? number? v_2)) (+ v_1 v_2)]
    [`(+ ,v_1 ,v_2) 
     `(+ ,(if (number? v_1) v_1 `(to-number ,v_1))
         ,(if (number? v_2) v_2 `(to-number ,v_2)))]
    
    [`(- ,(? number? v_1) ,(? number? v_2)) (- v_1 v_2)]
    [`(- ,v_1 ,v_2) 
     `(- ,(if (number? v_1) v_1 `(to-number ,v_1))
         ,(if (number? v_2) v_2 `(to-number ,v_2)))]
    
    [`(* ,(? number? v_1) ,(? number? v_2)) (* v_1 v_2)]
    [`(* ,v_1 ,v_2) 
     `(* ,(if (number? v_1) v_1 `(to-number ,v_1))
         ,(if (number? v_2) v_2 `(to-number ,v_2)))]
    
    [`(/ ,(? number? v_1) 0) #f]
    [`(/ ,(? number? v_1) ,(? number? v_2)) (/ v_1 v_2)]
    [`(/ ,v_1 ,v_2) 
     `(/ ,(if (number? v_1) v_1 `(to-number ,v_1))
         ,(if (number? v_2) v_2 `(to-number ,v_2)))]
    
    
    [`(% ,(? number? v_1) 0) #f]
    [`(% ,(? number? v_1) ,(? number? v_2)) (modulo v_1 v_2)]
    [`(% ,v_1 ,v_2) 
     `(% ,(if (number? v_1) v_1 `(to-number ,v_1))
         ,(if (number? v_2) v_2 `(to-number ,v_2)))]
    
    [`(#\. ,(? string? v_1) ,(? string? v_2)) (string-append (to-string v_1) (to-string v_2))]
    [`(#\. ,v_1 ,v_2) 
     `(#\. ,(if (string? v_1) v_1 `(to-string ,v_1))
           ,(if (string? v_2) v_2 `(to-string ,v_2)))]
    
    [`(or ,(? boolean? v_1) ,(? boolean? v_2)) (or v_1 v_2)]
    [`(or ,v_1 ,v_2) 
     `(or ,(if (boolean? v_1) v_1 `(to-bool ,v_1))
         ,(if (boolean? v_2) v_2 `(to-bool ,v_2)))]
    
    [`(and ,(? boolean? v_1) ,(? boolean? v_2)) (and v_1 v_2)]
    [`(and ,v_1 ,v_2) 
     `(and ,(if (boolean? v_1) v_1 `(to-bool ,v_1))
         ,(if (boolean? v_2) v_2 `(to-bool ,v_2)))]
    
    [`(=== ,v_1 ,v_2) (eq? v_1 v_2)]
    [`(!== ,v_1 ,v_2) (not (eq? v_1 v_2))]
    [`(== ,v_1 ,v_2) (cmp eq? v_1 v_2)]
    [`(!= ,v_1 ,v_2) (not (cmp eq? v_1 v_2))]
    [`(< ,v_1 ,v_2) (cmp < v_1 v_2)]
    [`(<= ,v_1 ,v_2) (cmp <= v_1 v_2)]
    [`(> ,v_1 ,v_2) (cmp > v_1 v_2)]
    [`(>= ,v_1 ,v_2) (cmp >= v_1 v_2)]
    
    [`(! ,(? boolean? v_1)) (not v_1)]
    [`(! ,v_1) `(! (to-bool ,v_1))]
    
    [`(- ,(? number? v_1)) (* v_1 -1)]
    [`(- ,v_1) `(- (to-number v_1))]
   
    [else (error "NYI")]))
