
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

it('add variable to variable', () => {
    var ws = { a: 1, b: 3 };
    var newws = run('add b to a.', ws);
    expect(newws.a).toEqual(4);
})

it('add two values with comma to variable', () => {
    var ws = { a: 1 };
    var newws = run('add 2, 3 to a.', ws);
    expect(newws.a).toEqual(6);
})

it('add two values to variable', () => {
    var ws = { a: 1 };
    var newws = run('add 2 3 to a.', ws);
    expect(newws.a).toEqual(6);
})

it('add two values to two variables', () => {
    var ws = { a: 1, b: 2 };
    var newws = run('add 2 3 to a b.', ws);
    expect(newws.a).toEqual(6);
    expect(newws.b).toEqual(7);
})

it('add two values to two variables with comma', () => {
    var ws = { a: 1, b: 2 };
    var newws = run('add 2 3 to a, b.', ws);
    expect(newws.a).toEqual(6);
    expect(newws.b).toEqual(7);
})

it('add two variables to variable', () => {
    var ws = { a: 1, b: 2, c: 3 };
    var newws = run('add b c to a.', ws);
    expect(newws.a).toEqual(6);
})

it('add with giving', () => {
    var ws = { a: 1, b: 2, c: 10 };
    var newws = run('add a b giving c.', ws);
    expect(newws.a).toEqual(1);
    expect(newws.b).toEqual(2);
    expect(newws.c).toEqual(3);
})

it('add with giving to two variables', () => {
    var ws = { a: 1, b: 2, c: 10, d: 11 };
    var newws = run('add a b giving c d.', ws);
    expect(newws.a).toEqual(1);
    expect(newws.b).toEqual(2);
    expect(newws.c).toEqual(3);
    expect(newws.d).toEqual(3);
})

it('add with giving to two variables with comma', () => {
    var ws = { a: 1, b: 2, c: 10, d: 11 };
    var newws = run('add a b giving c, d.', ws);
    expect(newws.a).toEqual(1);
    expect(newws.b).toEqual(2);
    expect(newws.c).toEqual(3);
    expect(newws.d).toEqual(3);
})

