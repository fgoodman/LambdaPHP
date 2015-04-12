#lang racket

(require 2htdp/batch-io
         php-parser
         "desugar.rkt"
         "../Redex/phpcore.rkt")

(define input (open-input-string (read-file 'stdin)))

(define λPHP (desugar (php-parse input)))

(trace ,λPHP)
