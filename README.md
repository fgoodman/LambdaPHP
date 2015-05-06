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
  * Strict equals: `1 === 5`
  * Strict not equals: `1 !== 5`
  * Loose equals: `2.2 == 3.5`
  * Loose not equals: `"test" != "foo"`
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
