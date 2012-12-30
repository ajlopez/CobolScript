
var cobs = require('../'),
    path = require('path'),
    assert = require('assert');
    
function compile(code, ws) {
    var program = cobs.compileProgram(code, true);

    if (ws) {
        program.data = program.data || { };
        program.data.working_storage = ws;
    }
    
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
assert.ok(text.indexOf('ws.a_1 = ws.a_2 = 1;') >= 0);

// simple function

var text = compile('procedure1.');
assert.ok(text.indexOf('function procedure1()') >= 0);

// function with moves

var text = compile('procedure1. move 1 to a. move 2 to b.', { a: null, b: null });
assert.ok(text.indexOf('function procedure1() {') >= 0);
assert.ok(text.indexOf('ws.a = 1;') >= 0);
assert.ok(text.indexOf('ws.b = 2;') >= 0);
assert.ok(text.indexOf('}') >= 0);

// function with parameters

var text = compile('procedure1 using x, y. move x to a. move y to b.', { a: null, b: null });
assert.ok(text.indexOf('function procedure1(x, y) {') >= 0);
assert.ok(text.indexOf('ws.a = x;') >= 0);
assert.ok(text.indexOf('ws.b = y;') >= 0);
assert.ok(text.indexOf('}') >= 0);

// function call with arguments

var text = compile('perform procedure1 using a, b.', { a: null, b: null });
assert.ok(text.indexOf('procedure1(ws.a, ws.b)') >= 0);

// perform procedure

var text = compile('perform procedure1.');
assert.ok(text.indexOf('procedure1();') >= 0);

// perform procedure with procedure

var text = compile('perform procedure1. procedure1. move 1 to a. move 2 to b.', { a: null, b: null });
assert.ok(text.indexOf('procedure1();') >= 0);
assert.ok(text.indexOf('function procedure1() {') >= 0);
assert.ok(text.indexOf('ws.a = 1;') >= 0);
assert.ok(text.indexOf('ws.b = 2;') >= 0);
assert.ok(text.indexOf('}') >= 0);

// perform procedure with varying

var text = compile('perform procedure1 varying k from 1 to 10 by 1. procedure1. move 1 to a. move 2 to b.', { k: null, a: null, b: null });
assert.ok(text.indexOf('for (ws.k = 1; ws.k <= 10; ws.k++)') >= 0);
assert.ok(text.indexOf('procedure1();') >= 0);
assert.ok(text.indexOf('function procedure1() {') >= 0);
assert.ok(text.indexOf('ws.a = 1;') >= 0);
assert.ok(text.indexOf('ws.b = 2;') >= 0);
assert.ok(text.indexOf('}') >= 0);

// perform procedure with giving

var text = compile('perform procedure1 giving k. procedure1. return 1.', { k: null });
assert.ok(text.indexOf('ws.k = procedure1();') >= 0);
assert.ok(text.indexOf('function procedure1() {') >= 0);
assert.ok(text.indexOf('return 1;') >= 0);
assert.ok(text.indexOf('}') >= 0);

// perform procedure with giving many variables

var text = compile('perform procedure-1 giving k, j. procedure-1. return 1.', { k: null, j: null });
assert.ok(text.indexOf('var $aux = procedure_1();') >= 0);
assert.ok(text.indexOf('ws.k = $aux;') >= 0);
assert.ok(text.indexOf('ws.j = $aux;') >= 0);
assert.ok(text.indexOf('function procedure_1() {') >= 0);
assert.ok(text.indexOf('return 1;') >= 0);
assert.ok(text.indexOf('}') >= 0);

// perform procedure with local

var text = compile('\
procedure division.\r\n\
perform procedure-1.\r\n\
perform procedure-2.\r\n\
\r\n\
procedure-1. local a.\r\n\
move 1 to a.\r\n\
return a.\r\n\
\r\n\
procedure-2. local a.\r\n\
move 2 to a.\r\n\
return a.'
, { k: null, j: null });
assert.ok(text.indexOf('procedure_1();') >= 0);
assert.ok(text.indexOf('procedure_2();') >= 0);
assert.ok(text.indexOf('function procedure_1() {') >= 0);
assert.ok(text.indexOf('function procedure_2() {') >= 0);
assert.ok(text.indexOf('var a;') >= 0);
assert.ok(text.indexOf('}') >= 0);

// if

var text = compile('if a > 0 then move 0 to a.', { a: null });
assert.ok(text.indexOf('if (ws.a > 0) {') >= 0);
assert.ok(text.indexOf('ws.a = 0;') >= 0);

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

// perform with in

var text = compile('perform func in obj');
assert.ok(text.indexOf('obj.func()') >= 0);

// use a procedure as parameter

var text = compile('perform proc1 using proc2. proc1. return 1. proc2. return 2.');
assert.ok(text.indexOf('proc1(proc2)') >= 0);

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
assert.ok(text.indexOf('a["foo"] = 1;') >= 0);

// perform with async

var text = compile('perform proc1 async. display "hello"');
assert.equal(text, 'proc1($cb1); function $cb1() { runtime.display("hello"); }');
text = compile('perform proc1 async giving a. display "hello". display a');
assert.equal(text, 'proc1($cb1); function $cb1(a) { runtime.display("hello"); runtime.display(a); }');
text = compile('perform proc1 async with error giving a. display "hello". display a');
assert.equal(text, 'proc1($cb1); function $cb1(err, a) { runtime.display("hello"); runtime.display(a); }');
text = compile('perform proc1 async with error. perform proc2 async with error giving a. display "hello". display a');
assert.equal(text, 'proc1($cb1); function $cb1(err) { proc2($cb2); function $cb2(err, a) { runtime.display("hello"); runtime.display(a); } }');

// procedure with async

var text = compile('procedure division. local name. perform proc1 async giving name. display "hello " name. proc1 async. return "world"');
assert.equal(text, 'var name; proc1($cb1); function $cb1(name) { runtime.display("hello ", name); } function proc1($cb) { $cb("world"); return; }'); 