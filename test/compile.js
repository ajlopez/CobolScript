
var cobs = require('../'),
    assert = require('assert');

// compile defined

assert.ok(cobs.compile);

// simple compile

var program = cobs.compile('display "hello".');

assert.ok(program);
assert.ok(program.command);

var text = program.command.compile(program);
assert.ok(text);
assert.ok(text.indexOf('runtime.display("hello");') >= 0);

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
