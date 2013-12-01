
var cobs = require('../');
    
function run(text, ws) {
    var program = cobs.compileProgram(text, ws);
    var data = program.run(null);
    return data.working_storage;
};

exports['subtract 1 from variable'] = function (test) {
    var ws = { a: 1 };
    var newws = run('subtract 1 from a.', ws);
    test.equal(ws.a, 1);
    test.equal(newws.a, 0);
};

exports['subtract 1 from two variables'] = function (test) {
    var ws = { a: 1, b: 2 };
    var newws = run('subtract 1 from a b.', ws);
    test.equal(ws.a, 1);
    test.equal(ws.b, 2);
    test.equal(newws.a, 0);
    test.equal(newws.b, 1);
};

exports['subtract 1 from variable giving variable'] = function (test) {
    var ws = { a: 1, b: 2 };
    var newws = run('subtract 1 from a giving b.', ws);
    test.equal(ws.a, 1);
    test.equal(ws.b, 2);
    test.equal(newws.a, 1);
    test.equal(newws.b, 0);
};

exports['subtract 1 from variable giving to two variables'] = function (test) {
    var ws = { a: 1, b: 2, c: 3 };
    var newws = run('subtract 1 from a giving b c.', ws);
    test.equal(newws.a, 1);
    test.equal(newws.b, 0);
    test.equal(newws.c, 0);
};

exports['subtract 1 + 2 from variable giving to two variables'] = function (test) {
    var ws = { a: 1, b: 2, c: 3 };
    var newws = run('subtract 1 2 from a giving b c.', ws);
    test.equal(newws.a, 1);
    test.equal(newws.b, -2);
    test.equal(newws.c, -2);
};

