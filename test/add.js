
var cobs = require('../');
    
function run(text, ws) {
    var program = cobs.compileProgram(text, ws);
    var data = program.run(null);
    return data.working_storage;
};

exports['add 1 to variable'] = function (test) {
    var ws = { a: 1 };
    var newws = run('add 1 to a.', ws);
    test.equal(newws.a, 2);
}

exports['add variable to variable'] = function (test) {
    var ws = { a: 1, b: 3 };
    var newws = run('add b to a.', ws);
    test.equal(newws.a, 4);
}

exports['add two values with comma to variable'] = function (test) {
    var ws = { a: 1 };
    var newws = run('add 2, 3 to a.', ws);
    test.equal(newws.a, 6);
}

exports['add two values to variable'] = function (test) {
    var ws = { a: 1 };
    var newws = run('add 2 3 to a.', ws);
    test.equal(newws.a, 6);
}

exports['add two values to two variables'] = function (test) {
    var ws = { a: 1, b: 2 };
    var newws = run('add 2 3 to a b.', ws);
    test.equal(newws.a, 6);
    test.equal(newws.b, 7);
}

exports['add two values to two variables with comma'] = function (test) {
    var ws = { a: 1, b: 2 };
    var newws = run('add 2 3 to a, b.', ws);
    test.equal(newws.a, 6);
    test.equal(newws.b, 7);
}

exports['add two variables to variable'] = function (test) {
    var ws = { a: 1, b: 2, c: 3 };
    var newws = run('add b c to a.', ws);
    test.equal(newws.a, 6);
}

exports['add with giving'] = function (test) {
    var ws = { a: 1, b: 2, c: 10 };
    var newws = run('add a b giving c.', ws);
    test.equal(newws.a, 1);
    test.equal(newws.b, 2);
    test.equal(newws.c, 3);
}

exports['add with giving to two variables'] = function (test) {
    var ws = { a: 1, b: 2, c: 10, d: 11 };
    var newws = run('add a b giving c d.', ws);
    test.equal(newws.a, 1);
    test.equal(newws.b, 2);
    test.equal(newws.c, 3);
    test.equal(newws.d, 3);
}

exports['add with giving to two variables with comma'] = function (test) {
    var ws = { a: 1, b: 2, c: 10, d: 11 };
    var newws = run('add a b giving c, d.', ws);
    test.equal(newws.a, 1);
    test.equal(newws.b, 2);
    test.equal(newws.c, 3);
    test.equal(newws.d, 3);
}

