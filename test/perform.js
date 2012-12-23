
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

// perform procedure

var ws = { a: 1 };
run('perform procedure1. procedure1. add 1 to a.', ws);
assert.equal(ws.a, 2);

// perform procedure with two commands

var ws = { a: 1, b: 3 };
run('perform procedure1. procedure1. add 1 to a. add 2 to b.', ws);
assert.equal(ws.a, 2);
assert.equal(ws.b, 5);

// perform two procedures

var ws = { a: 1, b: 3 };
run('perform procedure1. perform procedure2. procedure1. add 1 to a. procedure2. add 2 to b.', ws);
assert.equal(ws.a, 2);
assert.equal(ws.b, 5);

// perform procedure 10 times

var ws = { k: null, a: 0 };
run('perform procedure1 varying k from 1 to 4. procedure1. add k to a.', ws);
assert.equal(ws.a, 10);
assert.equal(ws.k, 5);

// perform procedure passing argument

var ws = { a: 1, b: 3 };
run('perform procedure1 using 3. procedure1 using x. add x to a. add x to b.', ws);
assert.equal(ws.a, 4);
assert.equal(ws.b, 6);
