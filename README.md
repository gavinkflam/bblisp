# bblisp

Hackable and side-effect free template language.

Influenced by Handlebars, Spacebars, LISP, Racket, Clojure and BBCode.

## Example

```
{{! Conditional blocks. }}
{{# if (defined? $name) }}Hello {{ $name }}!{{/#}}
{{# unless (defined? $name) }}Hello world!{{/#}}

{{! Flexible and optional specifications. }}
{{ assert $n (and (defined? $n) (integer? $n)) "n must be an integer" }}

{{! Expressive and customizable functions.
    'pluralize' is an example of user-defined Haskell function. }}
There {{# pluralize $n }}is{{/#}} {{ $n }} {{# pluralize $n }}apple{{/#}}.
```

## Goals

* Forbid side-effects.
* Lightning fast and minimal.
* Able to customize the language with syntactic forms and functions.
* Able to precompile templates for even faster rendering.

## Kernel Documentation

### Logical

#### and

`(and values:boolean...):boolean`

Returns true if all of the arguments are true.

Returns false if otherwise.

#### not

`(not value:boolean):boolean`

Returns the boolean complement of the argument.

#### or

`(or values:boolean...):boolean`

Returns true if any of the arguments is true.

Returns false if otherwise.

#### =

`(= values:any...):boolean`

Returns true if the arguments are of the same type and the values are
equivalent.

Returns false if otherwise.

### Arithmetic

#### +

`(+):integer`

`(+ values:integer...):integer`

`(+ values:integer/decimal...):decimal`

Returns the sum of the numbers. (+) returns 0.

#### -

`(-):integer`

`(- values:integer...):integer`

`(- values:integer/decimal...):decimal`

Subtracts the numbers from the first number.

(-) returns 0. (- x) returns the negation of x.

### String

#### str

`(str):string`

`(str value:any):string`

`(str values:any...):string`

With one argument, returns the string representation of `v`.

With more than one argument, returns the concatenation of the string
representations of each element.

### Collection

#### empty?

`(empty? collection:vector/dict/list/string):boolean`

Returns true if the argument has no items.

Vector, dictionary, list or string are supported.

#### get

`(get dictionary:dict key:string):any`

`(get vector:vector index:integer):any`

Returns the value mapped to the key or index.

Returns nil if the key or index is not present.

#### get-in

`(get-in dictionary:dict keys:vector[string/integer]):any`

`(get-in vector:vector keys:vector[string/integer]):any`

Returns the value in a nested associative structure using a sequence of
keys or indexes.

Returns nil if the key or index is not present.

#### member?

`(member? collection:vector/dict value:any):boolean`

Returns true if the given value is a member of the dictionary or vector.

Returns false if otherwise.

### Condition

#### if

`(if test:any then:any else:any):any`

`(if test:any then:any):any`

Evaluates `test`.

If it produces `true`, evaluate `then` and returns the result.

If it produces `false`, evaluate `else` and returns the result, or returns
`nil` when there are no `else`.

#### unless

`(unless test:any then:any else:any):any`

`(unless test:any then:any):any`

Evaluates `test`.

If it produces `false`, evaluate `then` and returns the result.

If it produces `true`, evaluate `else` and returns the result, or returns
`nil` when there are no `else`.

### Meta

#### eval

`(eval value:any):any`

Evaluate an expression or definition.

## Style Guide

* Strictly follow [hlint][hlint] suggestions.
* Strictly follow [tibbe/haskell-style-guide][haskell-style-guide].
* Strictly format all files with [stylish-haskell][stylish-haskell].
* Follow [Haskell programmig tips][haskell-programming-tips].
* Follow other modules.
* Consult [hlint coding style][hlint-github].
* Consult [Programming Guidelines][programming-guidelines].

## License

BSD3

[hlint]: https://hackage.haskell.org/package/hlint
[stylish-haskell]: https://hackage.haskell.org/package/stylish-haskell
[haskell-style-guide]: https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md
[haskell-programming-tips]: https://wiki.haskell.org/Haskell_programming_tips
[hlint-github]: https://github.com/ndmitchell/hlint
[programming-guidelines]: https://wiki.haskell.org/Programming_guidelines
