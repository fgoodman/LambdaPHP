#lang at-exp racket

(require php-parser
         redex
         "phpcore.rkt"
         "../LambdaPHP/desugar.rkt")

(define @php @string-append)

(define-syntax test
  (syntax-rules ()
    [(_ e1 e2)
     (test-predicate
      (lambda (result)
        (and (list? result) (= (length result) 1)
             (or (equal? (first result) (term e2))
                 (and (list? (first result))
                      (= 3 (length (first result)))
                      (equal? (third (first result)) (term e2))))))
      (apply-reduction-relation*
       eval-lambdaPHP
       (term ((()) () ,(desugar (php-parse (open-input-string
                                            (string-append "<?php "
                                                           e1))))))))]))

(test @php{(bool)false;} #f)
(test @php{(bool)0;} #f)
(test @php{(bool)0.0;} #f)
(test @php{(bool)"0";} #f)
(test @php{(bool)"";} #f)
(test @php{(bool)null;} #f)
(test @php{(bool)$x;} #f)
(test @php{(bool)true;} #t)
(test @php{(bool)-1;} #t)
(test @php{(bool)0.1;} #t)
(test @php{(bool)"null";} #t)
(test @php{(bool)true;} #t)

(test @php{"Hello" . " World";} "Hello World")
(test @php{"0" . 2 . 9 . true . "2";} "02912")
(test @php{1 . " World";} "1 World")
(test @php{1 . 5;} "15")
(test @php{true . 5.5;} "15.5")
(test @php{$foo = "Hello";
           $foo .= " World";
           $foo;} "Hello World")