# Notes
In this folder we will store notes. Notes serve to mitigate the readability problems that come with large comments.

Notes are also useful for describing general concepts and can be referred to multiple times.

## creating a note

we use `-- See Note [folder/file]` to refer to a note in this folder.

any note must start with 
```
Note [folder/file]
~~~~~~~~~~~~~~~~~~
```
so one can easily check they are at the right note.

optionally you can also add a header to the path:
`-- See Note [folder/file/header]`
we use this to refer to a specific place in the note.

# stuff i will forget:
## run tests and program
`cabal test && time cabal run`

## table of contents in markdown
`ctrl + shift + p` then Markdown All in One: Create Table of Contents.