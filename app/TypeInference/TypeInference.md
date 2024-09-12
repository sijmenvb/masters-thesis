# TypeInference

## algorithm
we use the standard type inferencing algorithm W

## implementation
### Inference monad
the Inference monad is effectively a state monad with a MaybeError to handle failure with error messages.

we have a liftError function to go from maybe's to this Inference monad so we can use maybe's in the same do block by lifting them.
## TODO's
- properly implement generalise function
- consider better algorithms for better errors