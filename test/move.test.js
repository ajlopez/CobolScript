
var cobs = require('../lib/cobolscript');
    
function run(text, ws) {
    var program = cobs.compileProgram(text, ws);
    program.procedure = program.compileFunction();
    var data = program.run(null);
    if (data)
        return data.working_storage;
    else
        return null;
};

it('compile and run move', () => {
    var ws = { a: null };
    var newws = run('move 1 to a.', ws);
    test.equal(newws.a, 1);
});

it('compile and run two moves', () => {
    var ws = { a: null, b: null };
    var newws = run('move 1 to a. move 2 to b.', ws);

    test.equal(newws.a, 1);
    test.equal(newws.b, 2);
});

it('compile and run two moves to nested items', () => {
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
});

it('compile and run move to two variables', () => {
    var ws = { a: null, b: null };
    var newws = run('move 1 to a, b.', ws);

    test.equal(newws.a, 1);
    test.equal(newws.b, 1);
});

