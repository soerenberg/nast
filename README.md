![ci](https://github.com/soerenberg/nast/actions/workflows/ci.yml/badge.svg)

# nAST
## Stan parser written in Haskell.
This is a parser for the [probabilistic programming Stan](https://mc-stan.org)
I wrote as  a small learning project. One goal was to allow for code comments
to be annotated to the AST nodes. This information might be used later in order
to capture instructions for linting, or to write a non-destructive code
formatter.

If I find the time in the future I might try to form this into a simple code
formatter or a transpiler to a different language / framework.

I also wrote a [Stan parser in Python](https://github.com/soerenberg/pynast).
