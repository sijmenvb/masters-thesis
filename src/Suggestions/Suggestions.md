# Suggestions

## generateExpressionSuggestion
the `generateExpressionSuggestion :: Type -> Maybe Type -> SuggestionBuilder [(WithSimplePos Expr, Type, SuggestionBuilderState)]` function is responsible for generating suggestions. 
it does this by iteratively eating one more argument. it returns each step.
example:
running it on `add 4 add 5 6`
would give us:
```
[ (add 4 (add 5 6) , Int              , state)
, (add 4           , Int -> Int       , state)
, (add             , Int -> Int -> Int, state)
]
```
where state is used to revert taking tokens
when it encountered the second `add` it was called recursively

### consuming input
TODO:
### swapping arguments
TODO: