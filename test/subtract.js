
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

// subtract 1 from variable

var ws = { a: 1 };
run('subtract 1 from a.', ws);
assert.equal(ws.a, 0);

// subtract 1 from two variables

var ws = { a: 1, b: 2 };
run('subtract 1 from a b.', ws);
assert.equal(ws.a, 0);
assert.equal(ws.b, 1);

// subtract 1 from variable giving variable

var ws = { a: 1, b: 2 };
run('subtract 1 from a giving b.', ws);
assert.equal(ws.a, 1);
assert.equal(ws.b, 0);