# bblisp

Hackable and side-effect free template language.

Influenced by Handlebars, Spacebars, LISP, Racket, Clojure and BBCode.

## Example

```
Hello {{# if $name}}{{$name}}{{## else }}world{{/#}}.
```

## Goals

* Forbid side-effects.
* Able to customize the language with syntactic forms and functions.
* Able to precompile templates for faster rendering.
* Stay fast and minimal.

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
