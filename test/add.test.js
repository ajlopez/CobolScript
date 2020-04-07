
var cobs = require('../lib/cobolscript');
    
function run(text, ws) {
    var program = cobs.compileProgram(text, ws);
    var data = program.run(null);
    return data.working_storage;
};

it('add 1 to variable', () => {
    var ws = { a: 1 };
    var newws = run('add 1 to a.', ws);
    expect(newws.a).toEqual(2);
})