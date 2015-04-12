#lang racket

(provide lambdaPHP-delta)

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

(define lambdaPHP-delta
  (match-lambda
    [`(to-bool ,v) (to-bool v)]
    [`(to-string ,v) (to-string v)]
    
    [`(#\. ,v_1 ,v_2) (string-append (to-string v_1) (to-string v_2))]
    
    ; http://php.net/manual/en/language.types.integer.php#language.types.integer.casting
    [`(to-int ,v) (match v
                    [#f 0]
                    [null 0]
                    [#t 1]
                    [(? flonum?) (floor v)]
                    [(? integer?) v]
                    ; http://php.net/manual/en/language.types.string.php#language.types.string.conversion
                    [(? string?) (error "todo")])]
    
    [`(=== ,v_1 ,v_2) (equal? v_1 v_2)]
    [`(< ,(? number? v_1) ,(? number? v_2)) (< v_1 v_2)]
    [`(+ ,(? number? v_1) ,(? number? v_2)) (+ v_1 v_2)]
    [`(- ,(? number? v_1) ,(? number? v_2)) (- v_1 v_2)]
    [`(- ,(? number? v)) (- 0 v)]
    [`(* ,(? number? v_1) ,(? number? v_2)) (* v_1 v_2)]
    [else (error "NYI")]))

(define (strtod str)
  ; optional whitespace (new line, tab, space)
  ; optional sign (+ or -)
  ; either (i) decimal number (ii) hexadecimal number (iii) infinity (iv) nan
  
  ; decimal number:
  ; nonempty decimal digits
  ; optional .
  ; decimal digits
  ; optional e (disregarding case)
  ; decimal digits

  ; inifity = INF or INFINITY (disregarding case)
  ; NAN = NAN (disregarding case) optinally followed by ( seq of char )
  null)