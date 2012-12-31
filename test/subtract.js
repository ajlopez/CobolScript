
var cobs = require('../'),
    assert = require('assert');
    
function run(text, ws) {
    var program = cobs.compileProgram(text, ws);
    var data = program.run(null);
    return data.working_storage;
};

// subtract 1 from variable

var ws = { a: 1 };
var newws = run('subtract 1 from a.', ws);
assert.equal(ws.a, 1);
assert.equal(newws.a, 0);

// subtract 1 from two variables

var ws = { a: 1, b: 2 };
var newws = run('subtract 1 from a b.', ws);
assert.equal(ws.a, 1);
assert.equal(ws.b, 2);
assert.equal(newws.a, 0);
assert.equal(newws.b, 1);

// subtract 1 from variable giving variable

var ws = { a: 1, b: 2 };
var newws = run('subtract 1 from a giving b.', ws);
assert.equal(ws.a, 1);
assert.equal(ws.b, 2);
assert.equal(newws.a, 1);
assert.equal(newws.b, 0);

// subtract 1 from variable giving to two variables

var ws = { a: 1, b: 2, c: 3 };
var newws = run('subtract 1 from a giving b c.', ws);
assert.equal(newws.a, 1);
assert.equal(newws.b, 0);
assert.equal(newws.c, 0);

// subtract 1 + 2 from variable giving to two variables

var ws = { a: 1, b: 2, c: 3 };
var newws = run('subtract 1 2 from a giving b c.', ws);
assert.equal(newws.a, 1);
assert.equal(newws.b, -2);
assert.equal(newws.c, -2);
