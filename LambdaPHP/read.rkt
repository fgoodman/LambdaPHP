#lang racket

(require 2htdp/batch-io
         php-parser
         "desugar.rkt"
         "../Redex/phpcore.rkt")

(define AST (php-parse (open-input-string (read-file 'stdin))))

(define λPHP (desugar AST))

(trace ,λPHP)
