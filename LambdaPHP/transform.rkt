#lang racket

(require php-parser)

(provide (all-defined-out))

(define ex1 (php-parse (open-input-string "<?php $x = 1;?>")))