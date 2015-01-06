# Lam

Lam is a small language based on the [λ-calculus][1].

[1]: http://en.wikipedia.org/wiki/Lambda_calculus

## Usage

Install with `cabal install`. Make sure `~/.cabal/bin` is in your PATH, and then type `lam` to start the Lam REPL. Alternatively, run `cabal build` and then `./dist/build/lam/lam`.

- Enter an expression to see it evaluated and printed.
- Enter an assignment to bind the name on the left-hand side to the result of evaluating the right-hand side. (The result will also be printed.)
- If you didn't assign the previous result to a name, you can still access it with the `%` symbol.
- Enter `load foo.lam` to evaluate the file `foo.lam`. It should only contain assignments since expressions will not be printed. You can also pass filenames as command line arguments to `lam` and they will be loaded on startup.
- When writing Lam in a file, if you need to continue an expression on another line, indent the continuation lines by at least one space or tab.
- You should probably load `prelude.lam` from this repository, since most of Lam itself (the language) is defined there.
- Enter `exit`, `quit`, or use Ctrl-C or Ctrl-D when you're finished.

## Features

- Takes minimalism to the extreme.
- Uses applicative order evaluation with lazy special form `If` for recursion.
- Parser error messages are easy to understand and useful.
- Command-line history (up/down arrow keys), navigation (left/right arrow keys), and other shortcuts such as Ctrl-A and Ctrl-E work as expected.
- Loading files is easy with tab completion.
- Church encoding of common data structures is built in.
- Non-feature: Lam is an extremely inefficient means of computation. That being said, you should hardly ever notice delays unless you are using Lam to crunch numbers.

## Grammar

Comments are ignored by Lam. They begin with `;` and continue until the end of the line. Blank lines and lines containing only whitespace are also ignored.

**Tokens** are the atomic units of Lam. A token is the only thing that cannot be reduced any further. A token is either one lowercase letter from `a` to `z`, or a string of characters not beginning with a lowercase letter. Specifically, the string can include any characters except whitespace and `()\.=`. For example, `Abc` is a single token while `abc` is actually three tokens side by side.

An **assignment** is of the form `x = e`, where `x` is a token that cannot contain any digit characters and `e` is an expression. The assignment binds `x` to the result of evaluating `e`.

### Expressions

There are three kinds of expressions:

- a symbol, defined by a token;
- a function, defined by a token and an expression;
- an application, defined by two expressions.

A token by itself is interpreted as a **symbol**. If that token has been bound to an expression previously, the symbol will evaluate to that expression. Otherwise, the symbol evaluates to itself (a terminal symbol). For example, if you enter `a = b`, then after that `a` will evaluate to `b` (whereas before it would just remain `a`). Evaluation continues as long as possible, so if you enter `b = c` next, then `a` will evaluate to `c`. It is impossible to create a loop in this way because `a = a` is a no-op (and if you enter `c = a`, the `a` will first evaluate to `c`, so it is the same as `c = c`).

A **function** is an expression of the form `\x.e`, where `x` is a token and `e` is any expression. The function _binds_ the variable `x`, so any occurrences of `x` in `e` refer to that bound variable. When a function is evaluated by itself, its body is reduced as much as possible.

An **application** is an expression of the form `f a`, where `f` and `a` are expressions. If `f` evaluates to a function, then the application is evaluated by substituting `a` for all occurrences of the formal parameter in the body expression of `f`. Otherwise, the expression remains an application object, with both terms reduced as much as possible.

### Rules

- Adding extra parentheses is always allowed: `(((a)))` is the same as `a`.
- Applications are left-associative, so `abc` is equivalent to `((a b) c)`.
- Function bodies extend as far to the right as possible, so `\x.x x` is the same as `\x.(x x)`, while explicit parentheses are needed for `(\x.x) x`.
- The application `f \x.x` is allowed, but be careful: `f \x.x \x.x` is the same as `f \x.(x \x.x)`, so you can only drop the parentheses from a function argument when it occurs at the end.
- The form `\xyz.e` is syntactic sugar for the curried function `\x.\y.\z.e`.

## Reduction

Lam uses applicative order evaluation. The only exception to this is the special form `If`. In the expression `If c a b`, the expressions `a` and `b` are never evaluated until `c` is a concrete Boolean value. Recursive functions must use `If` to work properly, otherwise evaluation will never end.

Lam does three basic types of reduction:

- **ɑ-conversion** renames the formal parameter of a function (including its occurrences in the function body). This is sometimes necessary to avoid unintentionally capturing free variables. For example, `\x.x` is ɑ-equivalent to `\y.y`.

- **β-reduction** is the function application: the substitution of an expression for all occurrences of the formal parameter in the body of the function. 

- **η-conversion** converts `\x.f x` to `f` whenever `x` does not occur as a free variable in `f`.

## Church encoding

Assuming the Prelude is loaded, `True` evaluates to `\xy.x` and `False` evaluates to `\xy.y`. Tokens consisting only of digits are handled specially by the interpreter; they are converted to Church numerals before evaluation. For example, `0` becomes `\fx.x` and `2` becomes `\fx.f (f x)`.

There are special functions that convert encoded data structures back into human-readable strings. The application `? b` evaluates to `True` if `b` represents logical truth and `False` for logical falsity. If `b` is not a Church boolean, the application is not reduced. Similarly, the application `# n` converts `n` into a string of digits if `n` can be interpreted as a Church numeral.

## Prelude

The Lam Prelude defines the core of the language:

- **Logic**: `True`, `False`, `And`, `Or`, `Not`
- **Arithmetic**: `Zero`, `Succ`, `Pred`, `Add`, `Sub`, `Mult`, `Div`, `Quot`, `Pow`
- **Comparison**: `Zero?`, `Eq?`, `LT?`, `GT?`, `LTEq?`, `GTEq?`, `Max`, `Min`
- **Lists**: `Cons`, `Car`, `Cdr`, `Nil`, `Nil?`, `Take`, `Drop`, `Map`, `Filter`, `Range`
- **Other**: `Id`, `Comp`

## Dependencies

Lam uses [Parsec][2] for parsing and [Haskeline][3] for the command-line interface.

[2]: https://hackage.haskell.org/package/parsec
[3]: https://hackage.haskell.org/package/haskeline

## License

© 2015 Mitchell Kember

Lam is available under the MIT License; see [LICENSE](LICENSE.md) for details.
