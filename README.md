# CobolScript

COBOL language compiler to Javascript. Work in Progress

## Installation

Via npm using (Node.js)[http://nodejs.org]:
```
npm install cobolscript
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
program.run(cobolscript.getRuntime());
```

Some programs need a runtime object that provides helper functions, i.e. `display` implementation. In the above example, a console-oriented runtime
object is obtained. 

In web samples, a runtime that uses request, response is used:
```js
http.createServer(function(req, res) {
    program.run(cobs.getRuntime({ request: req, response: res }));
}).listen(8000);
```

## Development

```
git clone git://github.com/ajlopez/CobolScript.git
cd CobolScript
npm install
npm test
```

## Samples

[Hello](https://github.com/ajlopez/CobolScript/tree/master/samples/hello) Simple Hello, world program, without division declarations.

[Hello Program](https://github.com/ajlopez/CobolScript/tree/master/samples/hellopgm) Hello, world program, with identification, environment, data and procedure divisions.

[Hello Web](https://github.com/ajlopez/CobolScript/tree/master/samples/helloweb) Hello web page. `display` output produces page content.

[Factorial](https://github.com/ajlopez/CobolScript/tree/master/samples/factorial) Factorial console program, using working storage variable, `perform` with `using` and `giving` and local variables for recursion.

[Factorial Web](https://github.com/ajlopez/CobolScript/tree/master/samples/factorialweb) Factorial web page.

[Local](https://github.com/ajlopez/CobolScript/tree/master/samples/local) Using `local` to define a variable, instead of working storage.

[Template](https://github.com/ajlopez/CobolScript/tree/master/samples/template) Console program using a template. CobolScript can be embedded in text.

[Template Web](https://github.com/ajlopez/CobolScript/tree/master/samples/templateweb) Web page using a template.

[Web Server](https://github.com/ajlopez/CobolScript/tree/master/samples/webserver) Accessing Node.js functions, to start a web server and serves a single page.

[Linkage Section](https://github.com/ajlopez/CobolScript/tree/master/samples/linkage) Using linkage section to access provided request, response in web page.

[Asynchronous Hello](https://github.com/ajlopez/CobolScript/tree/master/samples/helloasync) Perform and procedure using the new async reserved word.

[MySQL](https://github.com/ajlopez/CobolScript/tree/master/samples/mysql) Connecting and using a MySQL server.

[MySQL Web](https://github.com/ajlopez/CobolScript/tree/master/samples/mysqlweb) Connecting and using a MySQL server, generating web pages, listing databases, tables and columns.

[Web Site](https://github.com/ajlopez/CobolScript/tree/master/samples/website) Dynamic site using MySQL server, with customers and suppliers.

## To do

- More Samples
- Logical operators and expressions
- IS, NOT
- Comparison word operators
- Compute
- Complex expressions
- Pictures.
- File section.
- Database section.
- Copy
- `exec sql`.

## References

[COBOL Tutorial](http://www.mainframegurukul.com/tutorials/programming/cobol/cobol-tutorial.html)

[COBOL Tutorials](http://www.mainframetutorials.com/programming/programming.cobol.html)

[COBOL Programming Standards](http://www.tonymarston.net/cobol/cobolstandards.html)

## Contribution

Feel free to [file issues](https://github.com/ajlopez/CobolScript) and submit
[pull requests](https://github.com/ajlopez/CobolScript/pulls) — contributions are
welcome.

If you submit a pull request, please be sure to add or update corresponding
test cases, and ensure that `npm test` continues to pass.

