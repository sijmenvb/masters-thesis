# Type driven code suggestions
In this project we provide a proof of concept for generating code suggestions for invalid programs.

In short: we take some invalid code and see what the minimum* number of changes is to make it type check and thus compile. 

to do this we implemented these suggestions for a small subset of haskell

as an example take:
```
plus :: Int -> Int -> Int
stringRepeat :: Int -> String -> String

fun = stringRepeat "hello " plus 3 5)
```
we can generate the following suggestion:
<pre><code>did you mean:
fun = stringRepeat (plus 3 5) "hello "
--diff:--
fun = stringRepeat <span style="color:green;">(</span><span style="color:red;">str</span> plus 3 5) <span style="color:green;">str</span>
--Which has type: String
</code></pre>

this is meant as an addition to the normal error messages

this algorithm is able to recognise missing/misplaced brackets and flip arguments. As the title suggests these are generated based on the type system so we know that the final suggestion will type check.

features currently supported:
- fixing brackets
- swapping arguments
- lambda functions

## Key insights and assumptions
We assume that most programmers only make a few mistakes at a time. So we need to generate suggestions for mostly correct code.

A key insight is to split the code into sections early so we can fail late. While one function may not even get trough the parser we continue with the functions that do work. This allows us to get type information on the working functions wich can then be used to generate the suggestions.

Another key idea is to use the types the programmer gives to guide the suggestion generation.

## how to build
Open a terminal in the base directory and run:
```sh
cabal install alex
```
Then reboot the terminal (don't know if this is nessecairy).
Then run
```sh
cabal clean
cabal build
```
finally to run the program 
```
cabal run
```

use 
```
cabal test
```
to run the test cases.



# A bunch of examples:
TODO: add a bunch of examples here.