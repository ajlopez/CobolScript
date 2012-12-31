
var cobs = require('../'),
    path = require('path'),
    assert = require('assert');
    
function compile(code, ws) {
    var program = cobs.compileProgram(code, ws);
    return program.compileText();
}
    
function compileFile(filename, ws) {
    var program = cobs.compileProgramFile(filename, true);

    if (ws) {
        program.data = program.data || { };
        program.data.working_storage = ws;
    }
    
    return program.compileText();
}

// compile program defined

assert.ok(cobs.compileProgram);

// simple compile display

var text = compile('display "hello".');
assert.ok(text);
assert.ok(text.indexOf('runtime.display("hello");') >= 0);

// simple compile display from file

var text = compileFile(path.join(__dirname, '/files/hello.cob'));
assert.ok(text);
assert.ok(text.indexOf('runtime.display("hello");') >= 0);

// display two values

var text = compile('display "hello" "world".');
assert.ok(text);
assert.ok(text.indexOf('runtime.display("hello", "world");') >= 0);

// display with advancing

var text = compile('display "hello" "world" with advancing.');
assert.ok(text);
assert.ok(text.indexOf('runtime.display("hello", "world");') >= 0);

// display with no advancing

var text = compile('display "hello" "world" with no advancing.');
assert.ok(text);
assert.ok(text.indexOf('runtime.write("hello", "world");') >= 0);

// display no advancing

var text = compile('display "hello" "world" no advancing.');
assert.ok(text);
assert.ok(text.indexOf('runtime.write("hello", "world");') >= 0);

// display two values comma separated

var text = compile('display "hello", "world".');
assert.ok(text);
assert.ok(text.indexOf('runtime.display("hello", "world");') >= 0);

// simple compile move

var text = compile('move 1 to a-1.', {a_1: null});
assert.ok(text);
assert.ok(text.indexOf('ws.a_1 = 1;') >= 0);

// compile simple variable

text = compile('display A.', { a: null });
assert.ok(text);
assert.ok(text.indexOf('runtime.display(ws.a);') >= 0);

// compile simple variable in program

var text = compile('\
data division.\r\n\
working-storage section.\r\n\
01 a.\r\n\
procedure division.\r\n\
display a.\r\n\
');

assert.ok(text);
assert.ok(text.indexOf('runtime.display(ws.a);') >= 0);

// compile nested variable in program

var text = compile('\
data division.\r\n\
working-storage section.\r\n\
01 a.\r\n\
02 b.\r\n\
procedure division.\r\n\
display b.\r\n\
');
assert.ok(text);
assert.ok(text.indexOf('runtime.display(ws.a.b);') >= 0);

// simple compile two move commands

var text = compile('move 1 to a-1. move 2 to a-2.', { a_1: null, a_2: null });
assert.ok(text);
assert.ok(text.indexOf('ws.a_1 = 1;') >= 0);
assert.ok(text.indexOf('ws.a_2 = 2;') >= 0);

// simple compile two move commands without points

var text = compile('move 1 to a-1 move 2 to a-2', { a_1: null, a_2: null });
assert.ok(text);
assert.ok(text.indexOf('ws.a_1 = 1;') >= 0);
assert.ok(text.indexOf('ws.a_2 = 2;') >= 0);

// simple compile move to two variables

var text = compile('move 1 to a-1, a-2.', { a_1: null, a_2: null });
assert.ok(text);
assert.ok(text.indexOf('var $aux = 1; ws.a_1 = $aux; ws.a_2 = $aux;') >= 0);

// simple function

var text = compile('procedure1 section.');
assert.ok(text.indexOf('function procedure1()') >= 0);

// function with moves

var text = compile('procedure1 section. move 1 to a. move 2 to b.', { a: null, b: null });
assert.ok(text.indexOf('function procedure1() {') >= 0);
assert.ok(text.indexOf('ws.a = 1;') >= 0);
assert.ok(text.indexOf('ws.b = 2;') >= 0);
assert.ok(text.indexOf('}') >= 0);

// function with parameters

var text = compile('procedure1 section using x, y. move x to a. move y to b.', { a: null, b: null });
assert.ok(text.indexOf('function procedure1(x, y) {') >= 0);
assert.ok(text.indexOf('ws.a = x;') >= 0);
assert.ok(text.indexOf('ws.b = y;') >= 0);
assert.ok(text.indexOf('}') >= 0);


// if

var text = compile('if a > 0 then move 0 to a.', { a: null });
assert.ok(text.indexOf('if (ws.a > 0) {') >= 0);
assert.ok(text.indexOf('ws.a = 0;') >= 0);

// if with multiple commands

var text = compile('if a > 0 then move 0 to a move 1 to b.', { a: null, b: null });
assert.equal(text, 'if (ws.a > 0) { ws.a = 0; ws.b = 1; }');

// if with else

var text = compile('if a > 0 then move 0 to a else move 1 to b.', { a: null, b: null });
assert.equal(text, 'if (ws.a > 0) { ws.a = 0; } else { ws.b = 1; }');

// if with else end if

var text = compile('if a > 0 then move 0 to a else move 1 to b end-if move 2 to c.', { a: null, b: null, c: null });
assert.equal(text, 'if (ws.a > 0) { ws.a = 0; } else { ws.b = 1; } ws.c = 2;');

// return with value

var text = compile('return 1.');
assert.ok(text.indexOf('return 1;') >= 0);

// return

var text = compile('return.');
assert.ok(text.indexOf('return;') >= 0);

// move to nested item

var text = compile('move 0 to b in a', { a: { b: null } });
assert.ok(text.indexOf('ws.a.b = 0;') >= 0);

// declare local variable

var text = compile('local a.');
assert.ok(text.indexOf('var a;') >= 0);

// declare and use local variable

var text = compile('local a. move 1 to a');
assert.ok(text.indexOf('var a;') >= 0);
assert.ok(text.indexOf('a = 1;') >= 0);

// declare two local variables

var text = compile('locals a b.');
assert.ok(text.indexOf('var a, b;') >= 0);

// declare and use global variable

var text = compile('global a. move 1 to a');
assert.ok(text.indexOf('var a;') < 0);
assert.ok(text.indexOf('a = 1;') >= 0);

// compile global invocation

var text = compile('global foo. global require. perform require using "assert" giving foo.');
assert.ok(text.indexOf('foo = require("assert");') >= 0);

// a linkage item is visible as a runtime field

var text = compile('data division. linkage section. 01 a. procedure division. move 1 to a.');
assert.ok(text.indexOf('runtime.a = 1;') >= 0);

// call a linkage item as procedure

var text = compile('data division. linkage section. 01 response. procedure division. perform write in response.');
assert.ok(text.indexOf('runtime.response.write()') >= 0);

// move an empty object

var text = compile('local a. move object to a.');
assert.ok(text.indexOf('a = {};') >= 0);

// move an empty array

var text = compile('local a. move array to a.');
assert.ok(text.indexOf('a = [];') >= 0);

// move to an indexed array by name

var text = compile('local a. move 1 to a("foo")');
assert.ok(text.indexOf('runtime.setIndex(a, "foo", 1);') >= 0);

// stop run

var text = compile('stop run.');
assert.equal(text, 'runtime.stop(0);');

