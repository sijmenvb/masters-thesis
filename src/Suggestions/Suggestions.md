
- [Suggestions](#suggestions)
  - [Requirements](#requirements)
    - [Soft](#soft)
    - [Hard](#hard)
  - [generateSuggestion](#generatesuggestion)
  - [generateExpressionSuggestion](#generateexpressionsuggestion)
    - [calling generateExpressionSuggestion](#calling-generateexpressionsuggestion)
    - [consuming input](#consuming-input)
    - [swapping arguments](#swapping-arguments)
      - [note on indexing for swapping](#note-on-indexing-for-swapping)
      - [example with tree swaps:](#example-with-tree-swaps)
    - [addToAccumulator](#addtoaccumulator)
    - [lambda expression suggestions](#lambda-expression-suggestions)
      - [In detail:](#in-detail)
        - [get the current type environment](#get-the-current-type-environment)
        - [getting the arguments](#getting-the-arguments)
        - [consuming the -\>](#consuming-the--)
        - [adding arguments to the type environment](#adding-arguments-to-the-type-environment)
        - [generating the expression in the lambda](#generating-the-expression-in-the-lambda)
        - [building the type of the lambda expression](#building-the-type-of-the-lambda-expression)
        - [building the lambda expression](#building-the-lambda-expression)
        - [reverting the type environment](#reverting-the-type-environment)
        - [constructing the candidate](#constructing-the-candidate)
- [Token Difference](#token-difference)
  - [types to token streams](#types-to-token-streams)
  - [generating the differences](#generating-the-differences)
    - [performance](#performance)
- [Notable challenges](#notable-challenges)
  - [Indentation](#indentation)
      - [let parsing approach.](#let-parsing-approach)
        - [implementation details](#implementation-details)
        - [making the let selection](#making-the-let-selection)
  - [processing order](#processing-order)
  - [implementation](#implementation)
    - [processing a function out of order](#processing-a-function-out-of-order)

# Suggestions
in short suggestions are generated as follows. 
1. take the token stream with a problem.
2. generate a expression/section as an AST using these tokens and type information.
3. transform this suggestion back into (every possible) token stream that could generate this expression.
4. calculate the difference in tokens given and the ones generated as suggestion and present it to the user.

## Requirements
for generating our suggestions we have some soft and hard requirements
### Soft
if there was one mistake the suggestion should be able to fix it.
### Hard
If we take a valid function as a String and append a `(` to the end then giving this to the suggestion should generate the original string. (indentation might be slightly different, e.g. using 3 spaces instead of four) 
## generateSuggestion
`generateSuggestion :: Int -> Map.Map String Type -> [TokenInfo] -> MaybeError ([ExtendedTokens], String, String, Type, Int)`
is responsible for generating suggestions for function definitions. 
it takes:
- the current state for fresh variables.
- a type environment
- a token stream

It produces either an error or a collection of
- a list of expected tokens.
- a plain string representation of the final expression that could be copied.
- a string representation of the difference using colours.
- the type of the function
- the number of branches where we had to backtrack for performance. 

To do this we first split on the first `=` sign and peel off the function name. all the tokens to the left of the `=` will be the arguments while the tokens on the right will be used to generate a suggestion.

If the function we are trying to make a suggestion for has a type we assign the arguments the corresponding types and put those in the type environment. if there is no type we assign them free variables.

we take this type environment and use it to build a suggestionBuilder. 
we call `generateExpressionSuggestion` with the goal either a free variable or (If the function we are trying to make a suggestion for has a type) we give the type of the function subtracting all the types of the arguments. 

Here we still support functions like `myFun :: Int -> Int -> Int` where we only have `myFun x = ...` here the goal will simply be `Int -> Int` as only one argument is given in the function definition.

once the expression suggestions are generated we use the first valid suggestion (type matches goal) to build up our suggestion as a `Section` (the type for function definitions or type declarations).

finally we use some functions from `TokenDifference` to build the suggestion Strings. 

## generateExpressionSuggestion
the `generateExpressionSuggestion :: Type -> Maybe Type -> [Candidate] -> SuggestionBuilder [Candidate]` function is responsible for generating expression suggestions. 
here, a `Candidate` is defined as `type Candidate = (WithSimplePos Expr, Type, SuggestionBuilderState)`
Candidate holds suggested expressions and their type, it also holds the state of the program at the point it was generated for backtracking purposes.

candidates are generated by iteratively eating one more argument to to a function. It returns each candidate it has found.
example:
take 
```
trice :: (a -> a) -> a -> a
trice f x =  f (f (f x)) 
```
generating a suggestion for `trice plus 5 6`
would give us:
```hs
[ (trice (plus 5) 6 , Int               , state)
, (trice (plus 5)   , Int -> Int        , state)
, (trice            , (a -> a) -> a -> a, state)
]
```
where state is used to revert taking tokens
when it encountered the second `plus` it was called recursively with the remaining input `plus 5 6`
resulting in: 
```hs
[ (plus 5 6 , Int               , state)
, (plus 5   , Int -> Int        , state)
, (plus     , Int -> Int -> Int , state)
]
```
note how it did not take the most greedy result here since `Int` would not fit in the first argument of trice `(a -> a)`
### calling generateExpressionSuggestion
`generateExpressionSuggestion :: Type -> Maybe Type -> [Candidate] -> SuggestionBuilder [Candidate]` takes three arguments.
If you just want it to build a suggestion you can all it like so `generateExpressionSuggestion goal Nothing []` where goal is the type the suggestion should be. 

NOTE: the goal is mainly used for performance. it can still return candidates that do **not** match the goal! <!-- TODO: see if we need this --> 

the second and third arguments are used when plugging in arguments to a function.
the second argument `currentProcessType :: Maybe Type` holds the type of the function you are currently plugin arguments into. as soon as an argument is plugged in this type should be updated to reflect that. 

The third argument is the accumulator holing all the found candidates thus far.
### consuming input
consuming input starts by calling getExpr which will look at the next token (ignoring parenthesis) and returns it and its type as a candidate.

then one of two things will happen:
1. If the type of the candidate is some constant it will return the candidate as a singleton list.

2. If the type of the candidate is a function we switch the `generateExpressionSuggestion` into the function processing mode. meaning that we call it with the currentProcessType being `Just candidate_type` where candidate_type is the function type we just found.
Then we will call generateExpressionSuggestion normally to get the candidates for the first argument.
Then we try to fit the candidates into the first argument. We take the first argument we find that fits starting with the candidate that consumed the most tokens.
when it fits we use the substitution this generates and apply it to the return type of the function.
(Note: we take the view here that every function only takes one argument and the rest is the return type)
then this return type is the new currentProcessType for calling generateExpressionSuggestion.
we also construct the expression by taking the expression of the candidate with the fitting type and add it to the previously found expression that is currently in the accumulator.
this expression is added to the accumulator before calling generateExpressionSuggestion recursively.

the recursion stops when the currentProcessType is not a function and no more arguments could be plugged in. 

If no arguments fit we do not have to fail as we may partially apply functions. 
in this case we just return the accumulator. However we can do slightly better, if no arguments fit the programmer might have made a mistake here. So, we first check if the programmer accidentally swapped some arguments before returning the accumulator. more on this in the next section.  

### swapping arguments
Take `stringRepeat:: Int -> String -> String`
If a programmer types `stringRepeat "hello " 5` they probably misremembered the type of stringRepeat.
Rather than giving two type errors we want to give the suggestion that they should swap `"hello "` and `5`.
as seen in the previous section we try to do this when no candidates fit.

when no candidates fit the goal we recalculate the candidates with a free variable as to goal to make sure we can suggest any input.

we look at the first candidate and then we use `firstMatchToFront :: Type -> [Type] -> Maybe (Int, [Type])` to see from the list of all arguments if there is a match.
if there is no match anywhere we just try the next candidate.
If there is a match in any of the arguments we get the arguments reordered where the first match is now in the front. We also get the index which we'll use later to undo this swapping. 
we will hold this candidate for a while. Then we generate an expression as normal as if the currentProcessType is this new "reorderedType" type where we can just take off the first argument as we just saw that this fits. 
example for thus far:
for `stringRepeat "hello " 5` we see that we find a `"hello "::String` but that doesn't fit in `stringRepeat:: Int -> String -> String` we do see that if the arguments were `String -> Int -> String` it would fit. because we are going to hold `"hello "::String` to the side for a bit we continue generating as if we have just parsed `stringRepeat :: Int -> String` with the remaining input `5`. this will generate the following candidates:
```hs
[ (stringRepeat 5 , String                  , state)
, (stringRepeat   , Int -> String -> String , state)
]
```
now we do need to put the candidate we've been holding onto back into the type. 
We use the `insertIntoExpressionAtIndex :: Int -> WithSimplePos Expr -> WithSimplePos Expr -> Maybe (WithSimplePos Expr)` for that.
we can give it the index and if the generated expression is big enough for the candidate we've been holding to be inserted it will insert it. otherwise we get nothing.
in our example transform the candidates from above into:
```hs
[ (stringRepeat 5 "hello ", String          , state)
]
```
this removes too many candidates as the candidates generated before the swapping are still valid. so we just add the current accumulator back on the end.
this finishes our example:
```hs
[ (stringRepeat 5 "hello ", String          , state)
, (stringRepeat   , Int -> String -> String , state)
]
```

this approach works with a function with any number of arguments or required swaps.

#### note on indexing for swapping
`firstMatchToFront` should take the current goal to figure out the index.
for example if we have `fun :: String -> Bool -> Int -> Bool -> Int -> String`
and we've given the input `fun "hi" 4 5 True False` (the two booleans and two integers should be swapped here)
the first `hi` is applied then the `4` processed. we see that it doesn't fit and hold it for later. The new currentProcessType becomes `Bool -> Bool -> Int -> String` we need to process the `5` next, here it is important that we use this new currentProcessType so that we remove the right int. 
from `firstMatchToFront` we'll get index 2 as that is where it needs to be inserted in the currentProcessType. However, we already consumed a String in this case. So we need to keep the number of arguments already processed into account for generating the final index. since here a sting is already consumed as argument we need to add 1 to the index. 

In practice we do this by looking at the most greedy suggestion in our accumulator and counting how many arguments it has taken already.

#### example with tree swaps:
take `fun :: Bool -> Bool -> String -> Int -> Int -> String`
which we call with `fun 4 5 True "hi" False `
first we hold `4` (to be inserted at position 3) and recursively call as is the type is `Bool -> Bool -> String -> Int -> String`
then we hold `5` (to be inserted at position 3)  and recursively call as is the type is `Bool -> Bool -> String -> String`
then `True` just fits so we continue with `Bool -> String -> String`
then we hold `"hi"` (to be inserted at position 2) and recursively call as is the type is `Bool -> String`
finally `False` fits and we end up with the suggestion:
```hs
[ (fun True False , String                                         , state)
, (fun True       , Bool -> String -> String                       , state)
, (fun            , Bool -> Bool -> String -> Int -> Int -> String , state)
]
```
we add `"hi"` back in position 2
```hs
[ (fun True False "hi" , String                                         , state)
, (fun True            , Bool -> String -> String                       , state)
, (fun                 , Bool -> Bool -> String -> Int -> Int -> String , state)
]
```
we add `5` back in in position 3
```hs
[ (fun True False "hi" 5 , String                                         , state)
, (fun                   , Bool -> Bool -> String -> Int -> Int -> String , state)
]
```

finally we add `4` back in in position 3
```hs
[ (fun True False "hi" 4 5 , String                                         , state)
, (fun                     , Bool -> Bool -> String -> Int -> Int -> String , state)
]
```

### addToAccumulator

`addToAccumulator` adds the candidate to the accumulator. (regardless of the goal)
Its purpose is to make sure substitutions are handled.

If the candidate type matches the goal the substitution will also be applied to the candidates type and state.
It will revert the state of the suggestion builder to before this substitution so one can continue taking arguments.

<!-- TODO: add good example --> 

performance: maybe only if the function could end up in the goal instead of always.

### lambda expression suggestions
Lambda expressions are generated similarly to [`generateSuggestion`](#generatesuggestion). When we call the internal `getExpr` and find a lambda `\` we try to generate a lambda expression.

In short it works as follows:
1. get the current type environment to revert to later
2. get the tokens that represent the arguments
3. add the tokens to the type environment
4. generate an expression (and type) with this new environment
5. read the types from the arguments from the now updated type environment
6. use the generated expression and type with the arguments and their types to build up a return type and lambda expression that form a candidate.
7. revert the type environment for arguments

#### In detail:
##### get the current type environment
we first get the current type environment. A change in the type environment within a lambda has no effect outside said lambda. So we need to store the current type environment to revert to it later.
##### getting the arguments
We first look at the goal. This is done since we can make better informed decisions when the type of the lambda is known. for example if we know the type should be `Int -> Int -> Int` we know the lambda can take at most two arguments.
Note: if the type is known to be `Int -> Int -> a` it can still have an arbitrary number of arguments since `a` can be a function itself. also, if the type is `Int -> Int -> Int` we can also take fewer than two arguments. for example look at `(\x -> plus x)` which is a lambda of type `Int -> Int -> Int` that only takes one argument in the lambda definition.

If we know the type and it does not end in a variable we can stop taking arguments whenever we've reached the number of arguments. So if we know the desired type is e.g. `Int -> Int -> Int` and the given expression is 
```hs 
(\x y plus x y)
```

then we know that after the `y` we have consumed the two arguments and can go on to consume a `->`. 
##### consuming the ->
in the example above the `->` is missing. so we only consume the `->` if it exists if it does not we just assume the programmer missed it. 

we also stop taking arguments as soon as we encounter something that is not a label this means we stop taking arguments as soon as we see a `->` but also if we see a `=` or some other symbol. Sine we still only try to consume the `->` if there is some other symbol instead we still assume the `->`. and since our building of suggestions ignores unknown tokens it will suggest to remove "other symbol" instead. 
##### adding arguments to the type environment
the expression after the `->` within a lambda can use the arguments of the lambda so they need to be added to teh type environment. 
the arguments can be assigned to free variables. However, we can (sometimes) do better. If we know the goal we already know what the types of the arguments should be. Giving this information helps if there are other mistakes in the lambda like arguments that should be swapped.

to do this we use `addArgumentsToTypeEnvironment :: [Type] -> [TokenInfo] -> SuggestionBuilder [Type]` it takes the type-arguments for the goal and the arguments as tokens and adds them to the type environment if we run out of type arguments we use free variables. It also returns the Types that were not used.
##### generating the expression in the lambda
we use the normal way of calling `generateExpressionSuggestion` for the goal we can combine the unused arguments and the return type of the goal

note: if there "is no goal" the goal will be a free variable. which is also the `goalReturnType`.

This will generate a list of candidates for the body of the lambda.
##### building the type of the lambda expression
we can build the type of the complete lambda if we know the type of the arguments and the type of the expression. 

we can het the types by looking the arguments up using the (now updated) type environment. 

then we can use the `buildTypeFromArguments` to construct the final type

We do this for each candidate expression generated and we'll continue with the first one that fits the goal. (starting with the most greedy version)

##### building the lambda expression
we can reuse the `buildLambdaExpression` to build the lambda from the arguments and generated expression. we also need to add the start position of the lambda. 

note that lambdas like `(\x y -> 4)` are interpreted as `(\x -> (\y -> 4))` internally


##### reverting the type environment
since we now have the lambda expression and it's type we can make sure we revert the type environment for the arguments.

we revert it only for the arguments defined by this lambda as they should not exist outside the lambda (unless they already did and the lambda shadowed them). we cannot revert the entire type environment as applications within the lambda can specialise types.

##### constructing the candidate
to finish up we take the current state (now with the old type environment) the lamda and its type to construct the candidate required.

# Token Difference
The tokenDifference module is responsible for converting build in types to token streams that could be used to generate them. 
and it is responsible for taking such streams and an actual stream and finding the smallest difference between the two.

## types to token streams
we introduce 
```hs
data ExtendedTokens
  = Require Token
  | Optional Token (Maybe Int)
```
where we either say we require a token or that a token is optional. optional tokens may have an id with them. if one chooses to use an optional token with such an id all others with that id must also be used.

as an example say we have the expression `plus 4 5` as an internal `Expr` we must be able to construct the possible token streams that can be used to generate this expression unfortunately there are multiple:
```hs
plus 4 5
(plus 4) 5
((plus 4) 5)
```
these are parsed identically. to encode this we get the following
```hs
Optional '(' Just 0 
Optional '(' Just 1
Required "plus"
Required 4
Optional ')' Just 1
Required 5
Optional ')' Just 0 
```
This is only a slight simplification as `(((plus) (4)) 5)` would also be valid. but that makes the example unwieldy. 

It turns out these are relatively straightforward to generate from the dataTypes.

## generating the differences
a list of `ExtendedTokens` does not make an suggestion, we want to respect unnecessary parenthesis and want to suggest a token stream that is a s close to the original (faulty) token stream as possible.

to do this we use `generateActions :: [ExtendedTokens] -> [Token] -> [Action Token]`
it takes the `ExtendedTokens` and the original tokens and gives us a list of action tokens.
```hs
data Action a
  = Keep a
  | Add a
  | Remove a
```
an action will tell you if a token should be added to the original token stream, it it should be kept or if one should be removed. 

we do this in a similar manor to how the Levenshtein distance is usually implemented. 
for each token we effectively check if we should keep remove or add it. removing or adding increases the distance. then we take the minimum of all these options. 

### performance
to generate this difference we do not always decide to add or remove, if the required token and the token in the original is the same we just keep it without looking at deletion or addition. 

we also use dynamic programming, meaning we store intermediate results because the recursive algorithm often takes two different routes to the same path. this sped up the algorithm **MASSIVELY**. 

in general we believe this difference calculating is currently way more computationally intensive than generating the suggestion in the first place. 

# Notable challenges
## Indentation
haskell has indentation sensitive syntax. namely let and case expressions. 
We want to be able to correct mistakes in indentation. yet it is vital we do not make assumptions that can change a valid expression into a different valid expression. Otherwise we can get cases where the body of a let changes while there was only a missing parenthesis in the `in` part. 

we examplify this by a particularly difficult case. take:

```hs
plus x y = x + y

triFun :: Int -> Int -> Int -> Int

fun x = 
  let 
    fun2 x y = triFun x y
     plus x y = modulo (x + y) 5
  in fun2 (plus x 7)
```
here the problem is an extra space in front of plus. 

However once we get to the definition of fun2 if we try to build a suggestion from there normally we get
` fun2 x y = triFun x y (plus x y) :: Int`
being left with the token stream ` = modulo (x ...` which will not build a suggestion. even worse, if the indentation was correct we would have the same issue as we currently ignore any indentation in building the suggestion for an expression.

One big culprit in this failure is the name shadowing. if the plus in the let would not exists already the plus will not be consumed by the expression suggestion, thus the expression would parse properly.

this sounds like the same problem would occur when normally parsing multiple functions. there we solve it by having a pre processing step that looks at the tokens and splits them into functions based on indentation. so what we might want is some kind of pre-processor that can look at the body of our let and can split up the function definitions. 

Unfortunately we cannot rely on indentation again as that is what we are trying to fix.  

another problem with splitting syntactically is nesting. Take:
```hs
plus x y = x + y

triFun :: Int -> Int -> Int -> Int

fun x = 
  let 
    fun2 x y =
      let 
        plus x y = modulo (x + y) 5
      in 
        plus x y
    myVar = 7
  in 
    fun2 x myVar)
```
if we just look for the pattern indicating a new local definition (newline, indent, list of names, equals sing , rest) we would split on the `plus x y = ` in the nested let as well. although for the first let this should not be done as the plus definition belongs to the nested let.


our solution is a hybrid approach between parsing till stuck and splitting beforehand. as soon as we encounter a let, we split the tokens into sections that start with a (indent and) as list of names followed by an `=`. then this list is carried trough the program and will be eaten by any (nested) let expressions. 



 TODO: split on "[names] =" pattern. give list of potential local definitions recursively in state, make let build expressions one by one. parse on remaining tokens later.


| approach                                     | Can fix alternative definitions | out of oder dependencies | supports nesting | correct types with nesting | indentation insensitive | no extra bookkeeping recursively |
|----------------------------------------------|---------------------------------|--------------------------|------------------|----------------------------|--------------------------|----------------------------------|
| token-by-token approach                      |                                 |                          | X                | X                          | X                        | X                                |
| naive splitting                              | X                               |                          |                  |                            | X                        | X                                |
| naive splitting, order independent           | X                               | X                        |                  |                            | X                        | X                                |
| naive splitting with dependency restrictions | X                               | X                        | X                | X                          | X                        |                                  |
| splitting on indentation                     | X                               | X                        | X                | X                          |                          | X                                |


#### let parsing approach.

the main problem is that we cannot know what the type environment in a definition of a Let is as we have not looked at the further definitions. for example if plus is defined at the top level and at the bottom of a let:

```hs
plus x y = x + y

fun x = 
  let 
    myVar = plus 7 8
    fun2 x y =
      let 
        plus x y = modulo (x + y) 5
      in 
        plus x y

    plus x y = modulo (x + y) 7
  in 
    fun2 x myVar)
```

our algorithem sees the following:
typenv: `plus :: Int -> Int -> Int`
tokenStreams: 
```
[[myvar, =, ...],
 [fun2, x, y, =, ...],
 [plus, x, y, =, ...],
 [plus, x, y, =, ...]]
```

in this case the bottom plus is the "correct" one as it matches the indentation. 
in this case it would not matter as all the `plus` have the same type. but imagine if they did not, how would our algorithm know which of the three is the correct one.

the only solution is to try all and pick the first configuration that allows for generating a suggestion. 

we can do better than trying all combinations. 

we can use the actual indentation to order the possibilities we will try we will put matching indents first, then lower indents, the closer the indent the sooner we'll try it. and then further indents. this way we will look at configurations matching the indent first. so if the mistake was somewhere else we immediately use the correct one.

anther improvement we can make is by registering which variables where accessed. so if we fail to build a suggestion for a section of code that does not use our shadowed variable `plus` we will not try again with a different type for the variable that wasn't even used.

to go even further we can only change our pick for which `plus` at the place where it was first used as everything before did not use `plus` to get to this point. 

to go even even further we can see if it breaks at a `plus` and figure out if one of the types 

##### implementation details

we can find the exact indent for the ordering by looking at the row of the start position of the token since our `TokenInfo` retains source position information.

for each let we need to 
1. split into sections
2. get the names of all the sections and get the duplicates (add special token for names already in the type environment).
3. sort all the duplicates on the indent 
4. get the next configuration that minimises indent changes.
5. this config tells which definitions belong to this let
6. add the things in this let to the type env to be computed as encountered (and save a copy to revert later)
7. get all the suggestions for the definitions.
8. get the definition for the in part (what is left for the last selected section)
9. if any of this fails go to step 4.



problems: 
- how to respect indentation of inner lets. as in how to prevent our solution from (logically equivalently) pulling all definitions from a nested let to the bigger one instead of just the one that was the problem.
- how to avoid invalid solutions where one of our definitions is left for a nested let that does not exist.
- how to get the type environment to depend on the suggestion builder that requires said type environment (maybe lazy evaluation is enough?, but how to detect the mutual recursion loop....)
- this tries all possibilities of selecting in case there is a nested let.
- (point 5) we recompute many suggestions after trying each configuration even though some sections might be unchanged.
- does not take into account where or why it (might have) failed. (e.g. if it fails because we try to use a function that doesn't exist and there is a configuration that does allow it we should only try those next)
- prevent suggestions like above to be too aggressive (maybe instead of pulling x out of a let we need to push y into it.) 

possible solutions: 
- check if any of the sections contains a let. then it will consume at least 1 or more of the following definitions but never leave gaps.
as in for the configuration only deselect x from the end and x_1,x_2,..x_n after the n sections with a let.
- for point 5, maybe only give a subset of the type environment containing all the words in the section so results can be cashed?
- have the ability to gather restrictions in our configuration for when e.g. a sub function uses a function that is unknown on the top level it must be included.
- this does not work: maybe only figure out of the next section should belong to this let.
because: we need to know if top levels are overwriten by a definition in a let.
 



##### making the let selection 
for each let we encounter all the let ts with in minus the first n after each subsecton conaining a let where 
1 <= n < length of let to the end of the list or next let. 

## processing order
the order in which we build suggestions is important. Each generated suggestion can add more restrictions on the types and the order in which we process will determine how we build the suggestions.

for example take:
```hs
flipNum :: Bool -> Int -> Int

myFun a b = 
  let
    x = flipNum a b
    y = flipNum b a
  in plus x y
```
here the types of `a` and `b` are being restricted by its uses in flipNum. however a mistake was made here since `a` (and `b`) are used both as a `Bool` and as an `Int`. one of these instances should have its arguments flipped. But which one??? 

## implementation

because we might need the type of an expression defined in tokens we have not seen yet we change our type environment from a `Map String Type` to a `Map String (Map Scope TypePromise)` where a 
`TypePromise` is either a type or some function that takes the current state and (updates it and) returns a type. 
`Scope` is a combination of a depth and a branch. the depth tells you which indent a type comes from. e.g. if you have a let where you define a `plus` that is also defined on a top level you will have two entries for `plus` in your type environment and you'd take the one with the highest depth. it is important to keep both so we can backtrack but still apply our substitutions globally.

the branch is needed in case we need information that has not been computed yet. the branch allows us to jump to a parallel type environment while still being able to apply substitutions on all environments.

as an example for why this is needed take:
```hs
plus :: Int -> Int -> Int

myFun a = 
  let
    fun1 plus = plus fun2 a
    fun2 = plus 4 a
  in plus x y
```
lets assume we work from top to bottom (this might differ in practice). First we look at `fun1` which takes a variable `plus` as argument shadowing it's existing type. Then we encounter fun2 which we have not seen yet. so we need to pause here and first do fun2. the problem is that fun2 needs `plus` but we just shadowed it in figuring out fun1. so we somehow need to backtrack to when we knew what `plus` was we could have taken the environment at the time this fun2 was added to the type environment, however in `fun2` we learn that `a` needs to be an Int. and this information should be relayed back to fun1. (similar examples the other way around)  




### processing a function out of order
  when doing an out of order computation, for the type environment we do:
  1. take the current type env
  2. split it into values smaller than the current scope and bigger than the current scope
  3. add the values smaller than the current scope to the environment under a new branch
  4. set the new branch to be the current one
  5. calculate the new suggestion
  6. remove the new branch from the type environment
  7. set back the current branch


we need to do this branching because 
e.g.
```hs
plus :: Int -> Int -> Int

same :: a -> a -> a
same x y = y

ignoreFst :: a -> b -> b
ignoreFst x y = y

myFun a = 
  let
    fun1 plus = ignoreFst (same a plus) (fun2 plus) -- is the last plus restricted to be an Int here because it should have the same type as `a` but we only figured out what a is when computing fun2 
    fun2 = plus 4 a
  in plus x y

``` 
here the problem is that with the algorithm above the definition of plus is not around when the substitution for a is found while looking at fun2.

the correct making of fresh variables assures we can keep applying substitutions globally over all branches.







