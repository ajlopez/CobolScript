
var cobs = require('../');
    
function run(text, ws) {
    var program = cobs.compileProgram(text, ws);
    program.procedure = program.compileFunction();
    var data = program.run(null);
    return data.working_storage;
};

exports['simple if'] = function (test) {
    var ws = { a: 1 };
    var newws = run('if a > 0 then move 0 to a.', ws);
    test.equal(newws.a, 0);
};

exports['if with two commands'] = function (test) {
    var ws = { a: 1, b: 2 };
    var newws = run('if a > 0 then move 0 to a move 1 to b.', ws);
    test.equal(newws.a, 0);
    test.equal(newws.b, 1);
};
