
var cobs = require('../'),
    assert = require('assert');
    
function run(text, ws) {
    var program = cobs.compileProgram(text, ws);
    program.procedure = program.compileFunction();
    program.run(null);
};

// compile and run move

var ws = { a: null };
run('move 1 to a.', ws);
assert.equal(ws.a, 1);

// compile and run two moves

var ws = { a: null, b: null };
run('move 1 to a. move 2 to b.', ws);

assert.equal(ws.a, 1);
assert.equal(ws.b, 2);

// compile and run two moves to nested items

var ws = {
        group: {
            items: {
                a: null,
                b: null
            }
        }
    };
        
run('move 1 to a. move 2 to b.', ws);

assert.equal(ws.group.items.a, 1);
assert.equal(ws.group.items.b, 2);

// compile and run move to two variables

var ws = { a: null, b: null };
run('move 1 to a, b.', ws);

assert.equal(ws.a, 1);
assert.equal(ws.b, 1);
