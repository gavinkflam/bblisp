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
