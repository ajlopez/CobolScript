
var cobs = require('../'),
    assert = require('assert');

// run defined

assert.ok(cobs.run);

// simple run

assert.equal(3, cobs.run("return 1+2;"));

// compile and run display

var program = cobs.compile('display "Hello".');
var text = program.command.compile(program);

var result = null;

var runtime = {
    display: function(msg) {
        result = msg;
    }
};

cobs.run(text, runtime);

assert.equal(result, "Hello");

// compile and run move

var program = cobs.compile('move 1 to a.');

program.data = {
    working_storage: {
        a: null
    }
};

var text = program.command.compile(program);

var result = null;

cobs.run(text, null, program);

assert.equal(program.data.working_storage.a, 1);

// compile and run two moves

var program = cobs.compile('move 1 to a. move 2 to b.');

program.data = {
    working_storage: {
        a: null,
        b: null
    }
};

var text = program.command.compile(program);

var result = null;

cobs.run(text, null, program);

assert.equal(program.data.working_storage.a, 1);
assert.equal(program.data.working_storage.b, 2);

