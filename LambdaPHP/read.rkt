#lang racket

(require 2htdp/batch-io)
(require php-parser)

(require "desugar.rkt")
(require "../Redex/phpcore.rkt")

(define AST (php-parse (open-input-string (read-file 'stdin))))

(define λPHP (desugar AST))

(trace ,λPHP)
