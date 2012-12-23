
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

// compile and run add 1 to variable

var program = cobs.compile('add 1 to a.');

program.data = {
    working_storage: {
        a: 1
    }
};

var text = program.command.compile(program);

var result = null;

cobs.run(text, null, program);

assert.equal(program.data.working_storage.a, 2);

// compile and run subtract 1 from variable

var program = cobs.compile('subtract 1 from a.');

program.data = {
    working_storage: {
        a: 1
    }
};

var text = program.command.compile(program);

var result = null;

cobs.run(text, null, program);

assert.equal(program.data.working_storage.a, 0);

// compile and run multiply 3 by variable

var program = cobs.compile('multiply 3 by a.');

program.data = {
    working_storage: {
        a: 2
    }
};

var text = program.command.compile(program);

var result = null;

cobs.run(text, null, program);

assert.equal(program.data.working_storage.a, 6);

// compile and run divide 3 into variable

var program = cobs.compile('divide 3 into a.');

program.data = {
    working_storage: {
        a: 6
    }
};

var text = program.command.compile(program);

var result = null;

cobs.run(text, null, program);

assert.equal(program.data.working_storage.a, 2);
