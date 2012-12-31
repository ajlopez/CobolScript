
var cobs = require('../'),
    assert = require('assert');

// compile and run display

var program = cobs.compileProgram('display "Hello".');

var result = null;

var runtime = {
    display: function(msg) {
        result = msg;
    }
};

program.run(runtime);

assert.equal(result, "Hello");

// compile and run multiply 3 by variable

var program = cobs.compileProgram('multiply 3 by a.', { a: 2 });
var data = program.run(null);
assert.equal(data.working_storage.a, 6);

// compile and run divide 3 into variable

var program = cobs.compileProgram('divide 3 into a.', { a: 6 });
var data = program.run(null);
assert.equal(data.working_storage.a, 2);
