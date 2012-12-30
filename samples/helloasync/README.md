# Hello Asynchronous Sample

The clasical program, using `perform ... async [with error]` and `display` verbs.

## Setup

Install [Node.js](http://nodejs.org).

## Run

With the command line:
```
node run hello.cob
```

The output:
```
Hello, World
```

hello.cob code:
```cobol
procedure division.
perform sayhello async.
local name.
perform getname async giving name.
display name.

sayhello async.
display "Hello, " with no advancing.

getname asynchronous.
return "World".
```

Usually a perform code like this:
```
procedure division.
perform sayhello.
display "World".

sayhello.
display "Hello, " with no advancing.
```

is compiled to:
```javascript
sayhello();
runtime.display("World");
function sayhello() {
   runtime.write("Hello ");
}
```

Instead a perform async code:
```
procedure division.
perform sayhello async.
display "World".

sayhello async.
display "Hello, " with no advancing.
```

is compiled using callbacks to:
```javascript
sayhello($cb1);
function $cb1() {
    runtime.display("World ");
}
function sayhello($cb) {
    runtime.write("Hello ");
    $cb();
}
```

You MUST use async in both places: at perform invocation and procedure declaration.

