
var cobs = require('../');
    
function run(text, ws) {
    var program = cobs.compileProgram(text, ws);
    program.procedure = program.compileFunction();
    var data = program.run(null);
    if (data)
        return data.working_storage;
    else
        return null;
};

exports['compile and run move'] = function (test) {
    var ws = { a: null };
    var newws = run('move 1 to a.', ws);
    test.equal(newws.a, 1);
};

exports['compile and run two moves'] = function (test) {
    var ws = { a: null, b: null };
    var newws = run('move 1 to a. move 2 to b.', ws);

    test.equal(newws.a, 1);
    test.equal(newws.b, 2);
};

exports['compile and run two moves to nested items'] = function (test) {
    var ws = {
            group: {
                items: {
                    a: null,
                    b: null
                }
            }
        };
            
    var newws = run('move 1 to a. move 2 to b.', ws);

    test.equal(newws.group.items.a, 1);
    test.equal(newws.group.items.b, 2);
};

exports['compile and run move to two variables'] = function (test) {
    var ws = { a: null, b: null };
    var newws = run('move 1 to a, b.', ws);

    test.equal(newws.a, 1);
    test.equal(newws.b, 1);
};

