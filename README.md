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

[Hello](samples/hello) Simple Hello, world program, without division declarations.

[Hello Program](samples/helloprg) Hello, world program, with identification, environment, data and procedure divisions.

[Hello Web](samples/helloweb) Hello web page. `display` output produces page content.

[Factorial](samples/factorial) Factorial console program, using working storage variable, `perform` with `using` and `giving` and local variables for recursion.

[Factorial Web](samples/factorialweb) Factorial web page.

[Local](samples/local) Using `local` to define a variable, instead of working storage.

[Template](samples/template) Console program using a template. CobolScript can be embedded in text.

[Template Web](samples/templateweb) Web page using a template.

[Web Server](samples/webserver) Accessing Node.js functions, to start a web server and serves a single page.

## To do

- More Samples
- Web Site with templates and layout, get and post processing
- `if` with `else`, `end-if`
- `perform` inline with `end-perform`, `until`, `with test`.
- explicit `stop run`.
- `exit`, `exit perform`.

## References

[COBOL Tutorial](http://www.mainframegurukul.com/tutorials/programming/cobol/cobol-tutorial.html)

## Contribution

Feel free to [file issues](https://github.com/ajlopez/CobolScript) and submit
[pull requests](https://github.com/ajlopez/CobolScript/pulls) — contributions are
welcome.

If you submit a pull request, please be sure to add or update corresponding
test cases, and ensure that `npm test` continues to pass.

