# Assumptions
This document serves to catalogue the assumptions we make in order to get our optimisations to work.

# Restrictions
## Mostly correct
we assume that once the program is given to the compiler it is mostly correct. Garbage in = Garbage out.
## Only type checked suggestions
We will only give code suggestions if the suggestion does not generate more/different type errors. 

This means that it is the responsibility of the compiler to check the suggested code. If this checking is not done (for any reason) the suggestion should not be given. This checking can also be done implicitly by how the error is generated, if we make the rules of the type system dictate how a suggestion is generated we may consider it as already checked.

# Syntax
## Separation of functions/declarations
we assume we can split a source file up into separate
- function calls
- Type declarations
- imports

We should be able to do this even though any of the individual components might not parse/type check.

We require this since we want to be able to recover parsing/type checking errors on one function by using the type information of the other correct functions (etc.) in the same file.

## Given Type signatures are correct
If a type of a function is explicitly given, irregardless of if the body pares/type checks, we will assume this type is correct for giving code suggestions.