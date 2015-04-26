![PHP: Bat out of Hell](http://i.imgur.com/qk0sUI1.jpg)
# LambdaPHP
Semantics for PHP

## Parsing PHP
I use the [php-parser](https://github.com/antoineB/php-parser) Racket library
for parsing PHP.

## Desugaring to λPHP
I wrote a [desugaring
function](https://github.com/fgoodman/lambdaPHP/LambdaPHP/desugar.rkt) to
desugar the abstract syntax trees produced by php-parser. The following syntax
is supported:
* Primitive values \(integer, double, string, boolean, null\)
* Variable assignment \(\-, \+=, \-=, \*=, /=, %=, \.=\)
* Binary operators \(\+, \-, \*, /, %, \., ||, &&, ===, \!==, ==, \!=, <, <=, >, >=\)
* Unary operators \(\-, \!\)
* Casts (bool, int, float, string)
* For loops
* Function application
* Function declaration
* Global statements
* If statements
* Infix increment/decrement
* Postfix increment/decrement
* Return statements
* Ternary test operator
* While statements

## Visualizing reduction sequences
TODO
