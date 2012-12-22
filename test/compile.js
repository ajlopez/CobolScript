
var cobs = require('../'),
    assert = require('assert');

// compile defined

assert.ok(cobs.compile);

// simple compile display

var program = cobs.compile('display "hello".');

assert.ok(program);
assert.ok(program.command);

var text = program.command.compile(program);
assert.ok(text);
assert.ok(text.indexOf('runtime.display("hello");') >= 0);

// simple compile move

var program = cobs.compile('move 1 to a-1.');

program.data = {
    working_storage: {
        a_1: null
    }
};

assert.ok(program);
assert.ok(program.command);

var text = program.command.compile(program);
assert.ok(text);
assert.ok(text.indexOf('ws.a_1 = 1;') >= 0);

// compile simple variable

var program = cobs.compile('display A.');

assert.ok(program);
assert.ok(program.command);

program.data = {
    working_storage: {
        a: null
    }
};

var text = program.command.compile(program);
assert.ok(text);
assert.ok(text.indexOf('runtime.display(ws.a);') >= 0);

// compile simple variable in program

var program = cobs.compile('\
data division.\r\n\
working-storage section.\r\n\
01 a.\r\n\
procedure division.\r\n\
display a.\r\n\
');

assert.ok(program);
assert.ok(program.command);
assert.ok(program.data);
assert.ok(program.data.working_storage);

var text = program.command.compile(program);
assert.ok(text);
assert.ok(text.indexOf('runtime.display(ws.a);') >= 0);

// compile nested variable in program

var program = cobs.compile('\
data division.\r\n\
working-storage section.\r\n\
01 a.\r\n\
02 b.\r\n\
procedure division.\r\n\
display b.\r\n\
');

assert.ok(program);
assert.ok(program.command);
assert.ok(program.data);
assert.ok(program.data.working_storage);

var text = program.command.compile(program);
assert.ok(text);
assert.ok(text.indexOf('runtime.display(ws.a.items.b);') >= 0);

// simple compile two move commands

var program = cobs.compile('move 1 to a-1. move 2 to a-2.');

program.data = {
    working_storage: {
        a_1: null,
        a_2: null
    }
};

assert.ok(program);
assert.ok(program.command);

var text = program.command.compile(program);
assert.ok(text);
assert.ok(text.indexOf('ws.a_1 = 1;') >= 0);
assert.ok(text.indexOf('ws.a_2 = 2;') >= 0);

// simple compile move to two variables

var program = cobs.compile('move 1 to a-1, a-2.');

program.data = {
    working_storage: {
        a_1: null,
        a_2: null
    }
};

assert.ok(program);
assert.ok(program.command);

var text = program.command.compile(program);
assert.ok(text);
assert.ok(text.indexOf('ws.a_1 = ws.a_2 = 1;') >= 0);
