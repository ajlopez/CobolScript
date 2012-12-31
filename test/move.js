
var cobs = require('../'),
    assert = require('assert');
    
function run(text, ws) {
    var program = cobs.compileProgram(text, ws);
    program.procedure = program.compileFunction();
    var data = program.run(null);
    if (data)
        return data.working_storage;
    else
        return null;
};

// compile and run move

var ws = { a: null };
var newws = run('move 1 to a.', ws);
assert.equal(newws.a, 1);

// compile and run two moves

var ws = { a: null, b: null };
var newws = run('move 1 to a. move 2 to b.', ws);

assert.equal(newws.a, 1);
assert.equal(newws.b, 2);

// compile and run two moves to nested items

var ws = {
        group: {
            items: {
                a: null,
                b: null
            }
        }
    };
        
var newws = run('move 1 to a. move 2 to b.', ws);

assert.equal(newws.group.items.a, 1);
assert.equal(newws.group.items.b, 2);

// compile and run move to two variables

var ws = { a: null, b: null };
var newws = run('move 1 to a, b.', ws);

assert.equal(newws.a, 1);
assert.equal(newws.b, 1);
