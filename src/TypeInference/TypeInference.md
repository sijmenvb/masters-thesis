# TypeInference

## algorithm
we use the standard type inferencing algorithm W

### annotations are holy
When a user has explicitly given types we assume these to be correct. We do check them by looking if the definition matches the annotation (and give an error if not). Even when the definition does not match we keep trusting the annotation to proceed with inferring the other types.

One can comment out the annotation to revert back the the inferred type.

In this we differ from Languages like Haskell which will ignore the Type annotations until the very end.

We chose this because we find errors in the definition to be more common than in the annotation. We suspect this approach especially useful in suggesting fixes for recursive functions (with a type annotation). Since we can immediately see if the recursive call is used correctly(as in that it type checks) since we have the type already.

## Implementation
### `Inference` monad
the Inference monad 
```hs
newtype Inference a = Inference {runState :: InferenceState -> (MaybeError a, [Problem], InferenceState)}
```
is Effectively a:
 - state monad, to pass along state such as the counter for making free variables
 - MaybeError to handle failure with a string as error message
 - a collector(list) for errors, this enables recovery and still seeing multiple errors

There are a few functions to manipulate the `Inference` monad:

`fail`, `getState` and `putState` are what you expect

`recover` is used to recover from an error. If there is an error it takes the given recoverVal as the new output (to get it out of the error state) and reverts to the internal state before the `Inference` was evaluated. It will also take the error and use the functionName that was given to make it into a `Problem` and adds it to the list of problems as not to loose the error information.
This is referred to as gathering an errors.


`<?>` is used to overwrite the error message, 
It is often used in combination with `liftError`
which is used to go from `Maybe` to the `Inference` monad so we can use maybe's in the same do block by lifting them.
example:
```
do
    foundType <-
      liftError (Map.lookup identifier typeEnv)
        <?> "could not find" ++ show identifier ++ "in the TypeEnvironment"
```
### inferring types of expressions
We implemented the standard W type inferencing algorithm as the`typeInference` function. This uses the Inference monad to store state for making fresh variables and to carry the errors trough the computation of the program.

It takes a type environment and an expression as input and will give you the type of the expression.

### the order of type inference
The `sortSectionsOnDependencies` function is used to order dependencies so they are inferred in the right order. This means that if a function e.g. `fun2` uses `fun1` in it's definition the type of `fun1`should be inferred first.

We do this by calculating the "depth" of each section and sorting on that so shallow functions are inferred first.

#### the depth of a section
The depth of a section is defined as follows:
- Type annotation get depth -2 as they should be loaded first.
- Functions that use no other functions get a depth of 0.
- Functions that do use other functions get a depth of one higher than the greatest depth of the functions it uses.
- a depth of -1 is reserved for functions that do not exist. so, if a function uses a function without definition (or type annotation)

a nice benefit of this approach is that we know functions with the same depth cannot depend on each other so in theory they could be processed in parallel.


### inferring the type environment    
The function `inferTypeEnvironment` is used to infer the type environment based on a list of sections. you can give a initial type environment, this can be used to give the types of standard operators.

First the given sections are ordered as explained above.
Then the type annotations are processed first. It just adds the accompanying type to the type environment. It will produce an error if it finds duplicate annotations.
then we go over the function definitions. there are two interesting cases
1. There was already a type associated with that name.
   In this case we will not change the type environment. Instead we do type checking where we infer the type of the expression and match it to the associated type. If they do not match we gather an error.  
2. There is no type associated with the name.
   In this case, we Infer the type of the expression.
   To do this we use the `typeInference` function we give it the current type environment where we add/overwrite the bindings associated with the function arguments with those pointing to fresh variables. to build up the final type we use the type we get from `typeInference` as the return type. we use the substitution we get from  `typeInference` on the free variables we made for the function arguments to get their actual types. Finally we put it together the arguments and return type like so: 
   ```
   arg1Type -> arg2Type -> ... -> returnType
   ```
Any errors will be gathered, if inferring causes an error that section is skipped and the typeEnv is restored to what it was before that section.

## TODO's
- properly implement generalise function
- consider better algorithms for better errors
- detect Mutual recursion (currently infinite loop!)