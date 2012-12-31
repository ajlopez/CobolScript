
var cobs = require('../'),
    assert = require('assert');
    
function run(text, ws) {
    var program = cobs.compileProgram(text, ws);
    program.procedure = program.compileFunction();
    var data = program.run(null);
    return data.working_storage;
};

// simple if

var ws = { a: 1 };
var newws = run('if a > 0 then move 0 to a.', ws);
assert.equal(newws.a, 0);

// if with two commands

var ws = { a: 1, b: 2 };
var newws = run('if a > 0 then move 0 to a move 1 to b.', ws);
assert.equal(newws.a, 0);
assert.equal(newws.b, 1);
