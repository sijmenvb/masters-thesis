# Parser

## Source

We are using the [megaparsec](https://hackage.haskell.org/package/megaparsec) library. The basic code we got from the [megaparsec tutorial](https://markkarpov.com/tutorial/megaparsec.html).

## Structure
In `Parser/ParserBase` we hook up the token stream to megaparsec and provide some basic parsers.
In `Parser/Parser` we will put the parser for our grammar.

## Grammar

## Oddities
### not matching exact names
`pToken` usually matches only exactly the token given, an exception is made for `Name String` tokens where we do not require a match on the String part. here we accept any name token. This is because there is no reason to match on a specific name since these names can always be changed. 


TODO: properly ignore indents/spaces on otherwise empty lines.