![PHP: Bat out of Hell](http://i.imgur.com/qk0sUI1.jpg)
# LambdaPHP
A tested semantics for a bit of PHP.

## Parsing PHP
I use the [php-parser](https://github.com/antoineB/php-parser) Racket library
for parsing PHP.

## Desugaring to λPHP
I wrote a [desugaring
function](https://github.com/fgoodman/lambdaPHP/LambdaPHP/desugar.rkt) to
desugar the abstract syntax trees produced by php-parser. The following syntax and functionality
is supported:
* Primitive values
  * Integers: `-5, 0, 1`
  * Doubles: `-5.3, 0.1729, 1.0`
  * Strings: `"Hello world!"`
  * Booleans: `true, false`
  * Null: `null`
* Variable assignment
  * Normal: `$x = ...`
  * Plus: `$x += ...`
  * Minus: `$x -= ...`
  * Times: `$x *= ...`
  * Divide: `$x /= ...`
  * Modulo: `$x %= ...`
  * Dot: `$x .= ...`
* Binary operators
  * Plus: `1 + 5`
  * Minus: `2 - 5`
  * Times: `3 * 4`
  * Divide: `10 / 3`
  * Modulo: `10 % 3`
  * Dot: `"hello" . " world"`
  * Or: `true || false`
  * And: `false && true`
  * Identical: `1 === 5`
  * Not identical: `1 !== 5`
  * Equals: `2.2 == 3.5`
  * Not equals: `"test" != "foo"`
  * Less than: `1 < 3`
  * Less than or equal: `5 <= 6`
  * Greater than: `1 > -5`
  * Greater than or equal: `3 >= 3`
* Unary operators
  * Minus: `-5`
  * Not: `!false`
* Casts
  * Boolean: `(bool)...`
  * Integer: `(int)...`
  * Float: `(float)...`
  * String: `(string)...`
* Type juggling: `"10 cats" + "1e3" . "hello" - true`
* For loops: `for ($i = 0; $i < 10; $i += 1) { ... }`
* While loops: `while (...) { ... }`
* Function application: `foo(), bar(1, 5, 3)`
* Function declaration (top-level only): `function bar($x, $y, $z) { ... }`
* Return statements: `function baz($x) { ... return $x; ... }`
* Global statements: `function foo() { global $x, $y; ... }`
* If statements: `if (...) { ... } else if (...) { ... } else { ... }`
* Ternary test operator: `... ? ... : ...`
* Built-in functions: `md5`
* Echo statement: `echo ...`

## Testing λPHP
I use both [automated and manual testing](https://github.com/fgoodman/LambdaPHP/Redex/test.rkt). λPHP stores `echo`ed output in a buffer. The `test` function evaluates the provided PHP program and compares the standard output from the PHP interpreter with the λPHP buffer. The `debug` function displays the λPHP evaluation trace for the provided PHP program.

Examples were either created or taken from PHP's documentation comments. λPHP assumes all input is valid PHP 5.5.

## Case Study: Comparison
### The Background
PHP performs comparison, and thus defines equality, using an [algorithm](http://php.net/manual/en/language.operators.comparison.php) defined in its documentation. The identical operator `$a === $b` returns `true` if `$a` is equal to `$b`, and they are of the same type. The equal operator `$a == $b` returns `true` if `$a` is equal to `$b` after type juggling. Since PHP lacks explicit types, the context of an expression determines its type. [Type juggling](http://php.net/manual/en/language.types.type-juggling.php) is the automatic conversion of values to types required by a particular context. When [converting of strings to numbers](http://php.net/manual/en/language.types.string.php#language.types.string.conversion), PHP determines the value by the initial portion of the string. If the string begins with valid numeric data, then the numeric data becomes the value. Otherwise, `0` becomes the value. This works in a manner similar to C's `strtod`, which includes an optional exponent part, like `1.5e3` or `1E3`.
### The Problem
PHP includes a builtin [md5](http://php.net/md5) function that consumes a string and produces a string representing the has as a 32-character hexadecimal number. Because hexadecimal numbers include `e`, the md5 function has a 3.91% (exactly 10/256) chance of generating a string beginning with a number `0-9` followed by `e`. If PHP compares two strings beginning with a number followed by `e`, it converts the strings to numbers and performs a numerical comparison. This means `md5('240610708') == md5('QNKCDZO')` evaluates to `true`. And so does `md5('aabg7XSs') == md5('aabC9RqS')`. And many, many other pairs of hashed strings. As shown with these comparisons, when two hashes begin with `0e`, they are equal no matter what sequence of letters follow!
But PHP developers know better than to use a hasing function for encryption, right? And to never use the equal operator? [These](http://webcheatsheet.com/php/md5_encrypt_passwords.php) [results](http://www.script-tutorials.com/advance-php-login-system-tutorial/) [from](http://www.hauntednipple.co.uk/creating-a-login-system-in-php-salting-and-md5-encrypting-your-users-passwords/) Googling "secure php login tutorial" show otherwise, despite the PHP documentation [warning](http://php.net/manual/en/faq.passwords.php#faq.passwords.fasthash) users hashing function are unsuitable for passwords.
### The Diagnosis
The [trace](http://i.imgur.com/YSSnfaR.png) helps elucidate this behavior. While the trace fails to outline the semantics of equality and type juggling, the Redex model contains this functionality as deterministic [Racket code](https://github.com/fgoodman/LambdaPHP/blob/f6bb62b6439c3689bff8dcfb696bd9dd47edbb0e/Redex/phpdelta.rkt#L60-102).

## Case Study: Precedence
### The Background
PHP defines the [operator precedence](http://php.net/manual/en/language.operators.precedence.php) of `+` as left associative. However, while writing PHP's semantics in PLT Redex, I noticed this didn't always appear true. I constructed the following example, which to my surprise, printed `2` instead of `1`.
```php
<?php
$x = 0;
function foo() { global $x; $x = 1; return 1; }
echo $x + foo();
```
### The Diagnosis
I decided to wrap the `$x` portion of the final statement in a function to see if PHP produced the same result.
```php
<?php
$x = 0;
function foo() { global $x; $x = 1; return 1; }
function bar() { global $x; return $x; }
echo bar() + foo();
```
This time, PHP produces the expected output of `1`. I hypothesized some form of A-normalization must be occuring with non-atomic expressions, so I wrote a [metafunction](https://github.com/fgoodman/LambdaPHP/blob/f6bb62b6439c3689bff8dcfb696bd9dd47edbb0e/Redex/phpcore.rkt#L77-93) to perform this on λPHP Redex terms. To my surprise, this fixed the problem across all tests.
