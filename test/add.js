
var cobs = require('../'),
    assert = require('assert');
    
function run(text, ws) {
    var program = cobs.compileProgram(text, ws);
    var data = program.run(null);
    return data.working_storage;
};

// add 1 to variable

var ws = { a: 1 };
var newws = run('add 1 to a.', ws);
assert.equal(newws.a, 2);

// add variable to variable

var ws = { a: 1, b: 3 };
var newws = run('add b to a.', ws);
assert.equal(newws.a, 4);

// add two values with comma to variable

var ws = { a: 1 };
var newws = run('add 2, 3 to a.', ws);
assert.equal(newws.a, 6);

// add two values to variable

var ws = { a: 1 };
var newws = run('add 2 3 to a.', ws);
assert.equal(newws.a, 6);

// add two values to two variables

var ws = { a: 1, b: 2 };
var newws = run('add 2 3 to a b.', ws);
assert.equal(newws.a, 6);
assert.equal(newws.b, 7);

// add two values to two variables with comma

var ws = { a: 1, b: 2 };
var newws = run('add 2 3 to a, b.', ws);
assert.equal(newws.a, 6);
assert.equal(newws.b, 7);

// add two variables to variable

var ws = { a: 1, b: 2, c: 3 };
var newws = run('add b c to a.', ws);
assert.equal(newws.a, 6);

// add with giving

var ws = { a: 1, b: 2, c: 10 };
var newws = run('add a b giving c.', ws);
assert.equal(newws.a, 1);
assert.equal(newws.b, 2);
assert.equal(newws.c, 3);

// add with giving to two variables

var ws = { a: 1, b: 2, c: 10, d: 11 };
var newws = run('add a b giving c d.', ws);
assert.equal(newws.a, 1);
assert.equal(newws.b, 2);
assert.equal(newws.c, 3);
assert.equal(newws.d, 3);

// add with giving to two variables with comma

var ws = { a: 1, b: 2, c: 10, d: 11 };
var newws = run('add a b giving c, d.', ws);
assert.equal(newws.a, 1);
assert.equal(newws.b, 2);
assert.equal(newws.c, 3);
assert.equal(newws.d, 3);

