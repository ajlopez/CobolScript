
var cobs = require('../lib/cobolscript');
    
function run(text, ws) {
    var program = cobs.compileProgram(text, ws);
    var data = program.run(null);
    return data.working_storage;
};

it('subtract 1 from variable', () => {
    var ws = { a: 1 };
    var newws = run('subtract 1 from a.', ws);
    expect(ws.a).toEqual(1)
    expect(newws.a).toEqual(0)
});

it('subtract 1 from two variables', () => {
    var ws = { a: 1, b: 2 };
    var newws = run('subtract 1 from a b.', ws);
    expect(ws.a).toEqual(1)
    expect(ws.b).toEqual(2)
    expect(newws.a).toEqual(0)
    expect(newws.b).toEqual(1)
});

it('subtract 1 from variable giving variable', () => {
    var ws = { a: 1, b: 2 };
    var newws = run('subtract 1 from a giving b.', ws);
    expect(ws.a).toEqual(1)
    expect(ws.b).toEqual(2)
    expect(newws.a).toEqual(1)
    expect(newws.b).toEqual(0)
});

it('subtract 1 from variable giving to two variables', () => {
    var ws = { a: 1, b: 2, c: 3 };
    var newws = run('subtract 1 from a giving b c.', ws);
    expect(newws.a).toEqual(1)
    expect(newws.b).toEqual(0)
    expect(newws.c).toEqual(0)
});

it('subtract 1 + 2 from variable giving to two variables', () => {
    var ws = { a: 1, b: 2, c: 3 };
    var newws = run('subtract 1 2 from a giving b c.', ws);
    expect(newws.a).toEqual(1)
    expect(newws.b).toEqual(-2)
    expect(newws.c).toEqual(-2)
});

