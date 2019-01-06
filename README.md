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
