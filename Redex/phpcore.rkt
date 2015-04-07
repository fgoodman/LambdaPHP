#lang racket

(require redex)

(provide (all-defined-out))

(define-language lambdaPHP
        (sto ([loc val] ...))
        (loc natural)
        (prim boolean number string null)
        (val prim (lambda (x ...) e) (ref loc))
        (error (err val))
        (op + -)
        (e val
           error
           x
           (op e ...)
           (e e ...)
           (let ([x e] ...) e)
           (set! e e)
           (begin e e ...)
           (if e e e)
           (while e e))
        (E hole
           (op val ... E e ...)
           (val ... E e ...)
           (let ([x val] ... [y E] [z e] ...) e)
           (set! E e)
           (set! val E)
           (begin val ... E e ...)
           (if E e e)))
