
var cobs = require('../');

exports['compile and run display'] = function (test) {
    var program = cobs.compileProgram('display "Hello".');

    var result = null;

    var runtime = {
        display: function(msg) {
            result = msg;
        }
    };

    program.run(runtime);

    test.equal(result, "Hello");
};

exports['compile and run multiply 3 by variable'] = function (test) {
    var program = cobs.compileProgram('multiply 3 by a.', { a: 2 });
    var data = program.run(null);
    test.equal(data.working_storage.a, 6);
};

exports['compile and run divide 3 into variable'] = function (test) {
    var program = cobs.compileProgram('divide 3 into a.', { a: 6 });
    var data = program.run(null);
    test.equal(data.working_storage.a, 2);
};
