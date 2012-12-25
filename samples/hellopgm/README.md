# Hello Program Sample

The clasical program, using a complete declaration of divisions.

## Setup

Install [Node.js](http://nodejs.org).

## Run

With the command line:
```
node run hello.cob
```

The output:
```
Hello, world
```

hello.cob has the clasical four COBOL divisions declared:
```cobol
identification division.
    program-id. hello.
    author. A.J.Lopez.
    installation. test.
    date-written. 2012-12-22.
    date-compiled. 2012-12-22.
environment division.
    configuration section.
        source-computer. node.
        object-computer. node.
data division.
procedure division.
    display "Hello, world".
```
