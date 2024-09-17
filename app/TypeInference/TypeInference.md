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
The function `inferTypeEnvironment` is used to TODO: finish writing this.

## TODO's
- properly implement generalise function
- consider better algorithms for better errors
- detect Mutual recursion (currently infinite loop!)