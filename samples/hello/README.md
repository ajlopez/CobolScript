# Hello Sample

The clasical program, using `display` verb.

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

hello.cob is simple
```cobol
display "Hello, world".
```

With the command line:
```
node compile hello.cob
```

The output is:
```
runtime.display("Hello, world");
```

The runtime is an object provided by CobolScript implementation.

CobolScript doesn't enforce to have all the common COBOL declarations: identification division, environment
division, data division and procedure division. You can write directly the commands.

It supports a more complete program `hello2.cob`:
```cobol
 IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       PROCEDURE DIVISION.

       PROGRAM-BEGIN.
           DISPLAY "Hello world".

       PROGRAM-DONE.
           STOP RUN.
```
