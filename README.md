# CobolScript

COBOL language compiler to Javascript. Work in Progress

## Installation

Via npm using (Node.js)[http://nodejs.org]:
```
npm install basicscript
```

## Usage

Reference in your program:
```js
var cobolscript = require('cobolscript');
```

To compile a source code:
```js
var program = cobolscript.compileProgram('display "Hello, world".');
```

To compile a file:
```js
var program = cobolscript.compileProgramFile('./hello.cob');
```

To run a compiled program:
```js
program.run();
```

Some programs need a runtime object that provides helper functions, i.e. `display` implementation.
```js
program.run(runtime);
```

TBD: discuss runtime.

## Development

```
git clone git://github.com/ajlopez/CobolScript.git
cd CobolScript
npm install
npm test
```

## Samples

TBD

## To do

- Samples

## References

[COBOL Tutorial](http://www.mainframegurukul.com/tutorials/programming/cobol/cobol-tutorial.html)

## Contribution

Feel free to [file issues](https://github.com/ajlopez/CobolScript) and submit
[pull requests](https://github.com/ajlopez/CobolScript/pulls) — contributions are
welcome.

If you submit a pull request, please be sure to add or update corresponding
test cases, and ensure that `npm test` continues to pass.

