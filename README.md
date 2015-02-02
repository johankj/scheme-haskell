# Write Your Own Scheme
WYOS -- my take on the ‘Write Your Own Scheme Interpreter in Haskell’

For the month of January I decided to, for a small period each day, take my mind off of the finals and try to write my own Scheme Interpreter written in Haskell. This is the result.

Some of the exercises I implemented as part of the final interpreter, while others are only available as one of the Exercise.hs-files.

This was an easy project chosen to explore more of Haskell as well as a way to prepare for my Bachelor Project at Aarhus University, where I will be writing a compiler in a functional language.

## Usage

```bash
$ ghc --make Main.hs -o lisp
$ ./lisp file.scm
$ ./lisp
Lisp>>> (load "stdlib.scm")
Lisp>>> 
```
