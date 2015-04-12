#lang racket

(provide lambdaPHP-delta)

; http://perldoc.perl.org/perlretut.html#Non-capturing-groupings
(define (strtod str) ; close enough...
  (define m (regexp-match
             (pregexp
              "[+-]? *(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eE][+-]?\\d+)?") str))
  (if (= (length m) 1) (string->number (string-trim (first m))) 0))

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

(define lambdaPHP-delta
  (match-lambda
    [`(to-bool ,v) (to-bool v)]
    [`(to-int ,v) (to-int v)]
    [`(to-double ,v) (to-double v)]
    [`(to-string ,v) (to-string v)]
    
    [`(+ ,v_1 ,v_2) (+ (to-number v_1) (to-number v_2))]
    [`(- ,v_1 ,v_2) (- (to-number v_1) (to-number v_2))]
    [`(* ,v_1 ,v_2) (* (to-number v_1) (to-number v_2))]
    [`(/ ,v_1 ,v_2) (if (= v_2 0) false (+ (to-number v_1) (to-number v_2)))]
    [`(% ,v_1 ,v_2) (if (= v_2 0) false (+ (to-number v_1) (to-number v_2)))]
    
    [`(#\. ,v_1 ,v_2) (string-append (to-string v_1) (to-string v_2))]

    [`(or ,v_1 ,v_2) (or (to-bool v_1) (to-bool v_2))]
    [`(and ,v_1 ,v_2) (and (to-bool v_1) (to-bool v_2))]
    [`(=== ,v_1 ,v_2) (eq? v_1 v_2)]
    [`(!== ,v_1 ,v_2) (not (eq? v_1 v_2))]
    ;[`(== ,v_1 ,v_2)]
    ;[`(!= ,v_1 ,v_2)]
    [`(< ,v_1 ,v_2) (< (to-number v_1) (to-number v_2))]
    [`(<= ,v_1 ,v_2) (<= (to-number v_1) (to-number v_2))]
    [`(> ,v_1 ,v_2) (> (to-number v_1) (to-number v_2))]
    [`(>= ,v_1 ,v_2) (>= (to-number v_1) (to-number v_2))]
    
    [`(! ,v_1) (not (to-bool v_1))]
    [`(- ,v_1) (* (to-number v_1) -1)]
    
    ;[`(inc ,v_1)]
    ;[`(dec ,v_1)]
   
    [else (error "NYI")]))