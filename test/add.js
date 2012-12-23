
var cobs = require('../'),
    assert = require('assert');
    
function run(text, ws) {
    var program = cobs.compile(text);
    
    if (ws) {
        program.data = program.data || { };
        program.data.working_storage = ws;
    }
    
    var text = program.command.compile(program);
    var runtime = null;
    cobs.run(text, runtime, program);
};

// add 1 to variable

var ws = { a: 1 };
run('add 1 to a.', ws);
assert.equal(ws.a, 2);

// add variable to variable

var ws = { a: 1, b: 3 };
run('add b to a.', ws);
assert.equal(ws.a, 4);

// add two values with comma to variable

var ws = { a: 1 };
run('add 2, 3 to a.', ws);
assert.equal(ws.a, 6);

// add two values to variable

var ws = { a: 1 };
run('add 2 3 to a.', ws);
assert.equal(ws.a, 6);

// add two variables to variable

var ws = { a: 1, b: 2, c: 3 };
run('add b c to a.', ws);
assert.equal(ws.a, 6);

