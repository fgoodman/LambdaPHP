#lang at-exp racket

(require php-parser
         redex
         "phpcore.rkt"
         "../LambdaPHP/desugar.rkt")

(define (debug P)
  (traces L-reduce
          (term (IF (<?php ,(desugar (php-parse (open-input-string P))))))))

(define (test P)
  (define g (let-values ([(p out in err)
                          (subprocess #f #f #f "/usr/bin/php")])
              (display P in)
              (close-output-port in)
              (port->string out)))
  (define c (apply-reduction-relation*
             L-reduce
             (term (IF (<?php ,(desugar (php-parse (open-input-string P))))))))
  (if (and (list? c) (= (length c) 1))
      (if (string=? g (first (first c)))
          (begin
            (display "good\n")
            );#t)
          (begin
            (display "\nOutput does not match:\n")
            (display g)
            (display "\n")
            (display (first (first c)))
            ));#f))
      (begin
        (display "\nUnable to reduce:\n")
        (display c)
        )));#f)))


(define (generate-binary-operator-tests ops vals)
  (define inner (lambda (op)
                  (append-map (lambda (x)
                                (map (lambda (y)
                                       @string-append{echo @|x| @|op| @|y|; })
                                     vals))
                              vals)))
  (string-append "<?php " (apply string-append (append-map inner ops))))

(define (generate-unary-operator-tests ops vals)
  (define inner (lambda (op)
                  (map (lambda (x)
                         @string-append{echo @|op|(@|x|); })
                  vals)))
  (string-append "<?php " (apply string-append (append-map inner ops))))


#;(test (generate-binary-operator-tests 
       (list "+" "-" "*" #|"/"|# #|"%"|# "." "||" "&&" "===" "!==" "==" #|"!="|# "<" "<=" ">" ">=")
       (list "true" "false" "1" "0" "-1" "1.5" "\"1\"" "\"0\"" "\"-1\"" "\"1.5\"" "null" "\"php\"" "\"\"")))

#;(test (generate-unary-operator-tests
       (list "(bool)" "(int)" "(double)" "(string)" "!" #|"-"|#)
       (list "true" "false" "1" "0" "-1" "1.5" "1.0" "\"1\"" "\"0\"" "\"-1\"" "\"1.5\"" "\"1.0\"" "null" "\"php\"" "\"\"")))


(test @string-append{
<?php

echo "Hello World!";

echo 1, true, "Hello", null;

echo $n = null; // => null

echo $b1 = true; // => true
echo $b2 = false; // => false

echo $int1 = 12; // => 12
echo $int2 = -12; //  => -12
// $int3 = 012; // octal numbers are not supported (a leading 0 denotes an octal number)
// $int4 = 0x0F; // hexadecimal numbers are not supported (a leading 0x denotes a hexadecimal number)

echo $float1 = 1.234; // => 1.234
echo $float2 = 1.2e3; // => 1200.0
echo $float3 = 7E-10; // => 0.0000007
echo $float3 = -7E-10; // => -0.0000007

$string = "Bruno"; // => "Bruno"
$single = 'Hello $string'; // => "Hello $string" (single quotes do not allow embedding out variables)
$double = "Hello $string"; // => "Hello Bruno" (double quotes allow embedding out variables)
// nowdocs are not supported
})



(test @string-append{
<?php
echo (bool) "";        // bool(false)
echo (bool) 1;         // bool(true)
echo (bool) -2;        // bool(true)
echo (bool) "foo";     // bool(true)
echo (bool) 2.3e5;     // bool(true)
echo (bool) "false";   // bool(true)
})

(test @string-append{
<?php
function boolNumber($bValue = false) {                      // returns integer
  return ($bValue ? 1 : 0);
}

function boolString($bValue = false) {                      // returns string
  return ($bValue ? 'true' : 'false');
}

$a = true;                                                  // boolean value
echo 'boolean $a AS string = ' . boolString($a) . '<br>';   // boolean as a string
echo 'boolean $a AS number = ' . boolNumber($a) . '<br>';   // boolean as a number
echo '<br>';

$b = (45 > 90);                                             // boolean value
echo 'boolean $b AS string = ' . boolString($b) . '<br>';   // boolean as a string
echo 'boolean $b AS number = ' . boolNumber($b) . '<br>';   // boolean as a number
echo '<br>';

$c = boolNumber(10 > 8) + boolNumber(!(5 > 10));            // adding booleans
echo 'integer $c = ' . $c .'<br>';
})

(test @string-append{
<?php
// Consider that the 0 could by any parameters including itself
echo 0 == 1; // false
echo 0 == (bool)'all'; // false
echo 0 == 'all'; // TRUE, take care
echo 0 === 'all'; // false

// To avoid this behavior, you need to cast your parameter as string like that :
echo (string)0 == 'all'; // false
})

(test @string-append{
<?php
echo 1 + "10.5";                // $foo is float (11.5)
echo 1 + "-1.3e3";              // $foo is float (-1299)
echo 1 + "bob-1.3e3";           // $foo is integer (1)
echo 1 + "bob3";                // $foo is integer (1)
echo 1 + "10 Small Pigs";       // $foo is integer (11)
echo 4 + "10.2 Little Piggies"; // $foo is float (14.2)
echo "10.0 pigs " + 1;          // $foo is float (11)
echo "10.0 pigs " + 1.0;        // $foo is float (11)
})

(test @string-append{
<?php
echo '1.22' > '01.23'; // bool(false)
echo '1.22.00' > '01.23.00'; // bool(true)
echo '1-22-00' > '01-23-00'; // bool(true)
echo (float)'1.22.00' > (float)'01.23.00'; // bool(false)
})


(test @string-append{<?php
                     function factorial($n) {
                          if ($n == 0) return 1;
                          else return $n * factorial($n - 1);
                     }
                     echo $f = factorial(4);
                     
                     for ($i = 0; $i < $f; $i += 1)
                          echo $i;
                     })

; Variable substitution

; Primitive application
(test @string-append{<?php echo 1 + 2;})
(test @string-append{<?php echo "test" + 2;})
(test @string-append{<?php echo 1 + "test";})
(test @string-append{<?php echo "test" + "tset";})
(test @string-append{<?php echo "10 birds" + "5 cats";})
(test @string-append{<?php echo "5.5 mph" + 1.2;})
(test @string-append{<?php echo true + true;})
(test @string-append{<?php echo false + 2;})
(test @string-append{<?php echo null + 2;})
(test @string-append{<?php echo true + null;})
(test @string-append{<?php echo 1.5 + 2;})
(test @string-append{<?php echo 1 - 2;})
(test @string-append{<?php echo "test" - 2;})
(test @string-append{<?php echo 1 - "test";})
(test @string-append{<?php echo "test" - "tset";})
(test @string-append{<?php echo "10 birds" - "5 cats";})
(test @string-append{<?php echo "5.5 mph" - 1.2;})
(test @string-append{<?php echo true - true;})
(test @string-append{<?php echo false - 2;})
(test @string-append{<?php echo null - 2;})
(test @string-append{<?php echo true - null;})
(test @string-append{<?php echo 1.5 - 2;})
(test @string-append{<?php echo 1 * 2;})
(test @string-append{<?php echo "test" * 2;})
(test @string-append{<?php echo 1 * "test";})
(test @string-append{<?php echo "test" * "tset";})
(test @string-append{<?php echo "10 birds" * "5 cats";})
(test @string-append{<?php echo "5.5 mph" * 1.2;})
(test @string-append{<?php echo true * true;})
(test @string-append{<?php echo false * 2;})
(test @string-append{<?php echo null * 2;})
(test @string-append{<?php echo true * null;})
(test @string-append{<?php echo 1.5 * 2;})


; Casting
(test @string-append{<?php echo (bool)1;})
(test @string-append{<?php echo (bool)2.1;})
(test @string-append{<?php echo (bool)5.9;})
(test @string-append{<?php echo (bool)true;})
(test @string-append{<?php echo (bool)false;})
(test @string-append{<?php echo (bool)null;})
(test @string-append{<?php echo (bool)"turtles";})
(test @string-append{<?php echo (bool)"10 turtles";})
(test @string-append{<?php echo (bool)"15.3 turtles";})
(test @string-append{<?php echo (bool)"";})
(test @string-append{<?php echo (int)1;})
(test @string-append{<?php echo (int)2.1;})
(test @string-append{<?php echo (int)5.9;})
(test @string-append{<?php echo (int)true;})
(test @string-append{<?php echo (int)false;})
(test @string-append{<?php echo (int)null;})
(test @string-append{<?php echo (int)"turtles";})
(test @string-append{<?php echo (int)"10 turtles";})
(test @string-append{<?php echo (int)"15.3 turtles";})
(test @string-append{<?php echo (int)"";})
(test @string-append{<?php echo (double)1;})
(test @string-append{<?php echo (double)2.1;})
(test @string-append{<?php echo (double)5.9;})
(test @string-append{<?php echo (double)true;})
(test @string-append{<?php echo (double)false;})
(test @string-append{<?php echo (double)null;})
(test @string-append{<?php echo (double)"turtles";})
(test @string-append{<?php echo (double)"10 turtles";})
(test @string-append{<?php echo (double)"15.3 turtles";})
(test @string-append{<?php echo (double)"";})
(test @string-append{<?php echo (string)1;})
(test @string-append{<?php echo (string)2.1;})
(test @string-append{<?php echo (string)5.9;})
(test @string-append{<?php echo (string)true;})
(test @string-append{<?php echo (string)false;})
(test @string-append{<?php echo (string)null;})
(test @string-append{<?php echo (string)"turtles";})
(test @string-append{<?php echo (string)"10 turtles";})
(test @string-append{<?php echo (string)"15.3 turtles";})
(test @string-append{<?php echo (string)"";})

; Function application

; Variable assignment
(test @string-append{<?php echo $z;})
(test @string-append{<?php echo $z = 5;})
(test @string-append{<?php echo $x = $y = $z;})
(test @string-append{<?php $x = 5; $y = 6; $z = $x + $y; echo $z;})

; Global statement
(test @string-append{<?php function foo() { global $x; return $x; } echo foo();})
(test @string-append{<?php $x = 5; function foo() { global $x; echo $x; $x = 4; return $x; } echo foo(); echo $x;})
(test @string-append{<?php $x = 5; function foo() { return bar() + $x; } function bar() { global $x; $x = 1; return 5; } echo $x + foo();})

; Sequential statement
(test @string-append{<?php echo 1; echo 2; echo 3; echo 4;})

; Conditional statement
(test @string-append{<?php if (true) echo "yes"; else echo "no";})
(test @string-append{<?php if (false) echo "yes"; else echo "no";})
(test @string-append{<?php if (false) echo "yes"; else if (true) echo "maybe"; else echo "no";})
(test @string-append{<?php if (1 === 1) echo "yes"; else echo "no";})
(test @string-append{<?php if (1 === 0) echo "yes"; else echo "no";})
(test @string-append{<?php if (false) echo "yes"; else if (1 === 1) echo "maybe"; else echo "no";})

; While statement
(test @string-append{<?php $i = 0; while ($i < 5) $i = $i + 1; echo $i;})
(test @string-append{<?php while (false) $i = 5; echo $i;})

; Return statement
(test @string-append{<?php function foo() { return 1; } echo foo();})
(test @string-append{<?php function foo() { return 1; return 2; } echo foo();})
(test @string-append{<?php function foo() { $x = "hello"; return $x; } echo foo();})
(test @string-append{<?php function foo() { while (true) return true; } echo foo();})
(test @string-append{<?php function foo() { function bar() { return "baz"; } return bar(); } echo foo();})

; Echo statement
(test @string-append{<?php echo "Hello World!";})
(test @string-append{<?php echo 1;})
(test @string-append{<?php echo false;})
(test @string-append{<?php echo null;})

