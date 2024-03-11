# elm-review-codegen-simplify

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to simplify elm-codegen code.

## Provided rules

-   [`Codegen.Simplify`](https://package.elm-lang.org/packages/miniBill/elm-review-codegen-simplify/1.0.0/Codegen-Simplify) - Reports redundant uses of `Elm.value`, `Elm.apply`, `values_` and `call_`.

## Configuration

```elm
module ReviewConfig exposing (config)

import Codegen.Simplify
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ Codegen.Simplify.rule
    ]
```

## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template miniBill/elm-review-codegen-simplify/example
```
