# Lexer / Tokeniser

## Source
We use [Alex](https://haskell-alex.readthedocs.io/en/latest/) to generate the lexer.

This lexer was heavily influenced by [write-you-a-python-lexer](https://github.com/gdevanla/write-you-a-python-lexer) and it's accompanying [tutorial](https://gdevanla.github.io/posts/wya-lexer.html).



## Token information
All tokens are wrapped in a `TokenInfo`. 

This stores:
- The actual token defined by the `Token` data type in `Lexer.Tokens.hs`.
- The string that belongs to this token (e.g. DoubleColon has the string `"::"`).
- The starting line number and column number.
- The ending line number and column number.


## Special tokens

### Comment
The comment token stores the comments.
This includes:
- Single line (`-- I'm a comment :D`) comments.
- Multi-line (`{- I'm a comment too :P -}`) comments.
- Nested multi-line (`{- I'm a {- nested -} comment $) -}`) comments.

### Newline and NewlineAfterComment

`Newline` and `NewlineAfterComment` tokens indicate the end of a line.

The string that belongs to this token is the Newline and all the white space after it.

`NewlineAfterComment` tokens are only generated after comments that start on a line with no actual code.

example:
```haskell
val :: Int
val =
    -- this comment is on it's own line.
  42 -- this comment comes after some code
```
with tokens becomes
```haskell
val :: Int{-Newline-}
val = {-Newline-}
    -- this comment is on it's own line. {-NewlineAfterComment-}
  42 -- this comment comes after some code. {-Newline-}
```
This serves to make it easy to strip out comments and their newlines.

### Indent and Dedent
We generate `Indent` and `Dedent` tokens to keep track of changes in indentation.

example:
```haskell
fun x = case x of
  5 -> 
    something
    something_else
  0 ->
    something 
```
with tokens becomes
```haskell
fun x = case x of {-Newline-}
{-indent-} 5 -> {-Newline-}
    {-indent-}something {-Newline-}
              something_else {-Newline-}
{-dedent-}0 -> {-Newline-}
    {-indent-}something {-Newline-}
{-dedent-}{-dedent-}
```

`Indent` tokens share the "string that belongs to this token" information with the preceding `Newline`.

`Dedent` tokens have empty "string that belongs to this token" information.

If the file ends while indented **NO** `Dedent` tokens will be generated.

Empty lines and lines with (only) comments are ignored for the 


 <!-- TODO: explain the InternalAddIndentToPrevious --> 

## Running the lexer

The `Lexer.LexerRunner.hs` file contains the functions to run the lexer.

### runLexer

`runLexer :: String -> Either String [TokenInfo]` is the simplest way to run the lexer. You simply provide a `String` and it will either give you an error message as a `String` or the list of tokens wrapped in `TokenInfo`.

### runLexerOnFile
`runLexerOnFile` takes a file path instead of the string to process. 

### runAndPrettyPrintLexer

`runAndPrettyPrintLexer` will take a `String` and `print the tokens to the screen in the following format:
```
[start pos] - [end pos]      [token]     [string that belongs to this token ]
```

example: (not all tokens are implemented yet)
```haskell
fun1 :: Int -> Bool
fun1 x = True

val :: Int
val = 42

expression = fun1 val
```
gives us
```
1,0 - 1,4:           Name               "fun1"    
1,5 - 1,7:           DoubleColon        "::"      
1,8 - 1,11:          Name               "Int"     
1,12 - 1,14:         RArrow             "->"      
1,15 - 1,19:         Name               "Bool"    
1,19 - 2,0:          Newline            "\n"      
2,0 - 2,4:           Name               "fun1"    
2,5 - 2,6:           Name               "x"       
2,7 - 2,8:           EqualsSign         "="       
2,9 - 2,13:          Name               "True"    
2,13 - 3,0:          Newline            "\n"      
3,0 - 4,0:           Newline            "\n"      
4,0 - 4,3:           Name               "val"     
4,4 - 4,6:           DoubleColon        "::"      
4,7 - 4,10:          Name               "Int"     
4,10 - 5,0:          Newline            "\n"      
5,0 - 5,3:           Name               "val"     
5,4 - 5,5:           EqualsSign         "="       
5,6 - 5,8:           Number             "42"      
5,8 - 6,0:           Newline            "\n"      
6,0 - 7,0:           Newline            "\n"      
7,0 - 7,10:          Name               "expression"
7,11 - 7,12:         EqualsSign         "="       
7,13 - 7,17:         Name               "fun1"    
7,18 - 7,21:         Name               "val"     
7,21 - 8,0:          Newline            "\n"      
```