
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
