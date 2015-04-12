#lang racket

(provide lambdaPHP-delta)

(define lambdaPHP-delta
  (match-lambda
    
    ; http://php.net/manual/en/language.types.boolean.php#language.types.boolean.casting
    [`(to-bool ,v) (case v
                     [(#f 0 0.0 "" "0" null) #f]
                     [else #t])]
    ; http://php.net/manual/en/language.types.integer.php#language.types.integer.casting
    [`(to-int ,v) (match v
                    [#f 0]
                    [#t 1]
                    [(? flonum?) (floor v)]
                    [(? integer?) v]
                    ; http://php.net/manual/en/language.types.string.php#language.types.string.conversion
                    [(? string?) (error "todo")])]
    
    ; http://php.net/manual/en/language.types.string.php#language.types.string.casting
    [`(to-string ,v) (match v
                       [#t "1"]
                       [#f ""]
                       [(? null?) ""]
                       [(? number?) (number->string v)]
                       [(? string?) v])]
    [`(=== ,v_1 ,v_2) (equal? v_1 v_2)]
    [`(< ,(? number? v_1) ,(? number? v_2)) (< v_1 v_2)]
    [`(+ ,(? number? v_1) ,(? number? v_2)) (+ v_1 v_2)]
    [`(- ,(? number? v_1) ,(? number? v_2)) (- v_1 v_2)]
    [`(* ,(? number? v_1) ,(? number? v_2)) (* v_1 v_2)]))