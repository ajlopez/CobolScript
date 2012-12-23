
var cobs = require('../'),
    assert = require('assert');
    
function compile(code, ws) {
    var program = cobs.compile(code);

    if (ws) {
        program.data = program.data || { };
        program.data.working_storage = ws;
    }
    
    var text = program.command.compile(program);
    return text;
}

// compile defined

assert.ok(cobs.compile);

// simple compile display

var text = compile('display "hello".');
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
assert.ok(text.indexOf('runtime.display(ws.a.items.b);') >= 0);

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
assert.ok(text.indexOf('};') >= 0);

// function with parameters

var text = compile('procedure1 using x, y. move x to a. move y to b.', { a: null, b: null });
assert.ok(text.indexOf('function procedure1(x, y) {') >= 0);
assert.ok(text.indexOf('ws.a = x;') >= 0);
assert.ok(text.indexOf('ws.b = y;') >= 0);
assert.ok(text.indexOf('};') >= 0);

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
assert.ok(text.indexOf('};') >= 0);

// perform procedure with varying

var text = compile('perform procedure1 varying k from 1 to 10 by 1. procedure1. move 1 to a. move 2 to b.', { k: null, a: null, b: null });
assert.ok(text.indexOf('for (ws.k = 1; ws.k <= 10; ws.k++)') >= 0);
assert.ok(text.indexOf('procedure1();') >= 0);
assert.ok(text.indexOf('function procedure1() {') >= 0);
assert.ok(text.indexOf('ws.a = 1;') >= 0);
assert.ok(text.indexOf('ws.b = 2;') >= 0);
assert.ok(text.indexOf('};') >= 0);

// if

var text = compile('if a > 0 then move 0 to a.', { a: null });
assert.ok(text.indexOf('if (ws.a > 0) {') >= 0);
assert.ok(text.indexOf('ws.a = 0;') >= 0);

