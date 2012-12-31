
var cobs = require('../'),
    assert = require('assert');
    
function run(text, ws) {
    var program = cobs.compileProgram(text, ws);
    program.run(null);
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

// add two values to two variables

var ws = { a: 1, b: 2 };
run('add 2 3 to a b.', ws);
assert.equal(ws.a, 6);
assert.equal(ws.b, 7);

// add two values to two variables with comma

var ws = { a: 1, b: 2 };
run('add 2 3 to a, b.', ws);
assert.equal(ws.a, 6);
assert.equal(ws.b, 7);

// add two variables to variable

var ws = { a: 1, b: 2, c: 3 };
run('add b c to a.', ws);
assert.equal(ws.a, 6);

// add with giving

var ws = { a: 1, b: 2, c: 10 };
run('add a b giving c.', ws);
assert.equal(ws.a, 1);
assert.equal(ws.b, 2);
assert.equal(ws.c, 3);

// add with giving to two variables

var ws = { a: 1, b: 2, c: 10, d: 11 };
run('add a b giving c d.', ws);
assert.equal(ws.a, 1);
assert.equal(ws.b, 2);
assert.equal(ws.c, 3);
assert.equal(ws.d, 3);

// add with giving to two variables with comma

var ws = { a: 1, b: 2, c: 10, d: 11 };
run('add a b giving c, d.', ws);
assert.equal(ws.a, 1);
assert.equal(ws.b, 2);
assert.equal(ws.c, 3);
assert.equal(ws.d, 3);

