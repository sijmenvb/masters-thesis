# getArguments
getArguments will suggest the arguments for a function given that function's type. to do so it will consume the tokens.
## greedy
This function tries to return as many arguments as possible.

example: `plus 4 5` if our goal is some free type variable `v1` it could stop consuming arguments after 4. since the resulting type `Int -> Int` would match `v1` however since consuming another argument succeeds and `Int` also matches `v1` it will consume both 4 and 5.

## examples
### plus 4 5
for the function `plus :: Int -> Int -> Int` and the expression `plus 4 5` generateExpressionSuggestion first looks at `plus`. It will then try to gather as many arguments as it can. It will return[^1]
```hs
[ (Num 4, Int -> Int),
  (Num 5, Int       )]
```
[^1]:(we've taken a simplified view of the expression here, in actuality it is a proper `WithSimplePos Expr` )

where we get the expression of each argument (which can be an application on it's own)