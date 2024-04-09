# Parser

## Source

We are using the [megaparsec](https://hackage.haskell.org/package/megaparsec) library. The basic code we got from the [megaparsec tutorial](https://markkarpov.com/tutorial/megaparsec.html).

## Structure
In `Parsec/ParsecBase` we hook up the token stream to megaparsec and provide some basic parsers.
In `Parsec/Parsec` we will put the parser for our grammar.