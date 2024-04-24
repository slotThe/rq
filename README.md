# rq

`rq` is a tiny functional language with which you can manipulate JSON.
Basically, it is (an insignificant subset of!) [jq], written in Rust.

![Usage example](https://github.com/xmonad/xmonad-contrib/assets/50166980/3ba78375-b7de-4ca1-8dd5-e930fee0e85d)

[jq]: https://github.com/jqlang/jq

**NOTE**: This project is in its very early stages;
lot's of essential functions—and perhaps even syntax—might be missing,
and overall I can't guarantee that anything actually works.
Use at your own risk :)

## Installation

Use `cargo install`.
For nix users, a dev-shell is provided by the flake; one can access it with `nix develop`.
Additionally, you can run `rq` directly from the git repository:

``` console
$ nix run github:slotThe/rq
```

## Usage

Call `rq` with an expression, and pipe some JSON into it!

``` console
$ cat test.json
[{"name": "John Doe", "age": 43, "phones": ["+44 1234567", "+44 2345678"]}]

$ cat test.json | rq '\x -> x.0.phones.1'
+44 2345678
```

Some more usage examples:

``` console
$ cat simple.json
[{"name": "John Doe", "age": 43, "phone": +44 1234567"},{"name":"Alice"},{"name":"Bob", "age":42}]

$ cat simple.json | rq 'map .name'
["John Doe","Alice","Bob"]

$ cat simple.json | rq 'map .age | foldl (+) 0'
85

$ cat simple.json | rq 'filter (get "age" | (>= 42)) | map (\x -> { x.name: x.age })'
[{"John Doe":43},{"Bob":42}]

$ cargo metadata --format-version=1 | rq '.packages | map .name'
[ahash, allocator-api2, anyhow, ariadne, cc, cfg-if, chumsky, hashbrown, libc, once_cell, proc-macro2, psm, quote, rq, stacker, syn, unicode-ident, unicode-width, version_check, winapi, winapi-i686-pc-windows-gnu, winapi-x86_64-pc-windows-gnu, yansi, zerocopy, zerocopy-derive]
```

## The expression language

- Constants: `null, false, true, 1, 2.6, "string"`.

- Lambdas, which can be written in various ways:

        \x -> x        |x| x        λx → x

- Application is done via whitespace: `(\x -> x 1 2) const`.
  This would be akin to

        (|x| x(1, 2))(const)

   in pseudo-Rust notation (`const` is a [builtin function](#builtin-functions)).

- Binary operations:

  - Arithmetic operations, with the usual precedence rules of `*` and `/` being preferred over `+` and `-`:

          1 + 3 * 5 + 4 - 7    ≡    ((1 + (3 * 5)) + 4) - 7

    Additionally `+` also concatenates strings.

          "furble" + "wurble"    ≡    "furblewurble"

  - Comparison operations:

          1 = 3 * 5 + 4 - 7    ≡    1 = (((3 * 5) + 4) - 7)

          1 < 4 + 5 = 5        ≡    (1 < (4 + 5)) = 5

  The following table details the precedence rules:

    | Op                              | Precedence |
    |---------------------------------|------------|
    | `*`, `/`                        | 3          |
    | `+`, `-`                        | 2          |
    | `=`, `!=`, `<`, `<=`, `>`, `>=` | 1          |

- If-then-else expressions:

        if 5 = 2 + 3 then "wurble" else 4  ≡  if (5 = (2 + 3)) then "wurble" else 4
                                           ≡  "wurble"

- Arrays: `[1, 3, null]`.
  Arrays can contain arbitrary expressions:

        λx → [1, get 0 x, if false then 1 else 5]

- Objects: `{ "this": 3, "that": null }`.
  Apostrophes can be omitted:

        { this: 3, that: null }    ≡    { "this": 3, "that": null }

  In fact, keys can be arbitrary expressions—just make sure they actually evaluate to something sensible!

        { if true then "this" else "thus": 3, that: null }
          ≡  { "this": 3, "that": null }

- Expressions can be type-annotated with `::` or `∷`, though this is usually not necessary.

        λ> 1 + 2 :: JSON
        3

        λ> :e 1 + 2 + (3 ∷ JSON)
        + (+ 1 2) (3 ∷ JSON)

### Syntactic sugar

- The `get` function—with which one can index arrays and objects—can be abbreviated by `.`:

        λx → x.0.this     ≡    λx → get "this" (get 0 x)

        (λx → x.0.this) [{this: 4}]    ≡    4

  Additionally, `.0` is sugar for `(|x| x.0)`.
  This composes sanely:

        .0.1.2  ≡  λx → get 2 (get 1 (get 0 x))

  Note that this syntax is only available if the to-be-indexed-thing is a variable.

        [1, 2, 3].0     # Parse error!

- Instead of manually composing functions, `|` may be used instead;

        (get 0 | λx → { x.id: x.name }) [{id: 42, name: "Arthur"}, 4]
          ≡  { 42: Arthur }

- Lambdas can be written taking multiple arguments,
  in which case they are automatically curried:

        \x y -> x    ≡    \x -> \y -> x    ≡    |x, y| x

- A shadowed variable may be accessed using its De Bruijn index:

        λ> (λx → λx → x@2) 1 2
        variable not in scope: x@2
        λ> (λx → λx → x@1) 1 2
        1
        λ> (λx → λx → x@0) 1 2
        2
        λ> (λx → λx → x  ) 1 2
        2

- Various binary operators can be written in pettier/alternative ways:

  - Multiplication: `*`, `·`
  - Division: `/`, `÷`
  - Equality: `=`, `==`
  - Non-equality: `!=`, `/=`, `≠`
  - Less-or-equal: `<=`, `≤`
  - Bigger-or-equal: `>=`, `≥`

### The type system

`rq` is a statically typed language with subtyping.[^1]
The type system looks as follows:

- Primitive types: `JSON` and `Num`, where `Num ≤ JSON`.

- Universal quantification: `∀a. «Type»` or `forall a. «Type»`.
  The type variable `a` can be any valid identifier (that is not already a primitive type)

- Function types: `«Type» → «Type»` or `«Type» -> «Type»`.
  Function types are contravariant in their first, and covariant in their second argument.

- List types: `[«Type»]`.
  Lists are covariant.

### Standard library

- Numerical operators:

  ``` agda
  (+)  : JSON → JSON → JSON  -- Also works for string concatenation
  (-)  : JSON → JSON → JSON
  (*)  : JSON → JSON → JSON
  (/)  : JSON → JSON → JSON
  ```

- Comparisons:

  Essentially, everything that is not `false` or `null` is considered truthy.

  ``` agda
  (=)  : JSON → JSON → JSON
  (!=) : JSON → JSON → JSON
  (<)  : JSON → JSON → JSON
  (<=) : JSON → JSON → JSON
  (>)  : JSON → JSON → JSON
  (>=) : JSON → JSON → JSON
  ```

- Higher order functions:

  ``` agda
  -- `map f xs` applies `f` to every "value" in `xs`, which may be an
  -- array (in which case value means element), or an object (in which
  -- case it really means value).
  map : (JSON → JSON) → JSON → JSON

  -- Like map, `filter p xs` applies `p` to every value of `xs`.
  -- Keep the elements for which the predicate returns truthy.
  filter : (JSON → JSON) → JSON → JSON

  -- Left-associative fold over an array or (values of an) object; e.g.,
  --
  --   foldl f init [x₁, x₂, …, xₙ]  ≡  f(f(…f(init, x₁), …), xₙ)
  --
  foldl : (JSON → JSON → JSON) → JSON → JSON → JSON
  ```

- Misc

  ``` agda
  -- The identity function.
  id    : ∀a. a → a,

  -- Return the first argument
  const : ∀a. ∀b. a → b → a

  -- `get i x` gets the i'th thing out of x. I should be (evaluate to) a
  -- number or a string, with x evaluating to array or object, respectively.
  get   : JSON → JSON → JSON

  -- `set i x` coll sets the i'th thing in coll to x, where i
  -- should be a number or a string, and coll should evaluate to
  -- an array or object.
  set   : JSON → JSON → JSON → JSON
  ```

## REPL

A REPL is provided for getting familiar with the language;
either call `rq` without arguments,
or with a `repl` positional argument:

``` console
$ rq
λ>
```

By default, expressions will first be type-checked,
and then evaluated as far as they can:

``` console
λ> 1 + 2
3

λ> |x| x
λx'. x'

λ> \x -> x x
Occurs check: can't construct infinite type: b ≡ b → c

λ> \x -> ids x
variable not in scope: ids

λ> (get 0 | λx → { x.id: x.name }) [{id: 42, name: "Arthur"}, 4]
{ 42: Arthur }
```

Additionally, the following keywords are available:

  - Pretty-print the expression given
    (this just runs the parser, followed by the pretty-printer):
    `:e`

        λ> :e \x -> x x
        λx. (x x)

        λ> :e \x -> get 0 x + 3 * 5 - 7
        λx. (- (+ (get 0 x) (· 3 5)) 7)

  - Type-check an expression, and print the type: `:t`

        λ> :t \f -> \g -> \x -> f x (g x)
        (a → b → c) → (a → b) → a → c

        λ> :t \x -> get 0 x + 3 * 5 - 7
        JSON → JSON

        λ> :t map
        (JSON → JSON) → JSON → JSON

        λ> :t \x -> x x
        Occurs check: can't construct infinite type: b ≡ b → c

  - Get information on a builtin function with `:i`:

        λ> :i <
        e < e' checks whether e is less than e'

        λ> :i map
        map f xs applies f to every value in xs, which may be an
        array (in which case »value« means element) or an object
        (in which case it really means value).

  - To list all builtin functions, use `:l`:

        λ> :i map
        +	Add two number, or concatenate two strings.
        -	Subtract two numbers.
        <	e < e' checks whether e is less than e'
        … and so on …

  - Debugging: `:d`

        λ> :d \x -> x 4 "flurble"
        Lam("x", App(App(Var("x"), Const(Num(OrdF64(4.0)))), Const(String("flurble"))))

    + Prettier, yet more verbose, output: `:dp`

            λ> :dp \x -> x x
            Lam(
                "x",
                App(
                    Var(
                        "x",
                    ),
                    Var(
                        "x",
                    ),
                ),
            )

[^1]: This will be indicated by `SubType ≤ T`.
