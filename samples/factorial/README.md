# Factorial Sample

This sample shows the use of user defined functions.

## Setup

Install [Node.js](http://nodejs.org).

## Run

With the command line:
```
node run factorial.cob
```

The output:
```
1! = 1
2! = 2
3! = 6
4! = 24
5! = 120
6! = 720
7! = 5040
8! = 40320
9! = 362880
10! = 3628800
```

factorial.cob declares a variable in working storage section (without using picture, yet):
```cobol
data division.
working-storage section.
01 n.
```

The procedure division has a loop:
```cobol
procedure division.
perform show-factorial varying n from 1 to 10.
```

show-factorial procedure calls factorial using `using` to pass an argument, and `giving` to 
save the result. Then, it shows the result using `display`. Note the declaration and use of
a local variable `result`.
```cobol
show-factorial.
local result.
perform factorial using n giving result.
display n "! = " result.
```

factorial procedure invokes itself recursively
```cobol
factorial using n.
local m.
if n = 1 then return n.
subtract 1 from n giving m.
perform factorial using m giving m.
multiply n by m.
return m.
```