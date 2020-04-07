
var cobs = require('../lib/cobolscript');
    
function run(text, ws) {
    var program = cobs.compileProgram(text, ws);    
    program.procedure = program.compileFunction();
    var data = program.run(cobs.getRuntime());
    if (data)
        return data.working_storage;
    else
        return null;
};

it('perform procedure', () => {
    var ws = { a: 1 };
    var newws = run('perform procedure1. procedure1 section. add 1 to a.', ws);
    expect(newws.a).toEqual(2)
});

it('perform procedure with two commands', () => {
    var ws = { a: 1, b: 3 };
    var newws = run('perform procedure1. procedure1 section. add 1 to a. add 2 to b.', ws);
    expect(newws.a).toEqual(2)
    expect(newws.b).toEqual(5)
});

it('perform two procedures', () => {
    var ws = { a: 1, b: 3 };
    var newws = run('perform procedure1. perform procedure2. procedure1 section. add 1 to a. procedure2 section. add 2 to b.', ws);
    expect(newws.a).toEqual(2)
    expect(newws.b).toEqual(5)
});

it('perform procedure 10 times', () => {
    var ws = { k: null, a: 0 };
    var newws = run('perform procedure1 varying k from 1 to 4. procedure1 section. add k to a.', ws);
    expect(newws.a).toEqual(10)
    expect(newws.k).toEqual(5)
});

it('perform procedure passing argument', () => {
    var ws = { a: 1, b: 3 };
    var newws = run('perform procedure1 using 3. procedure1 section using x. add x to a. add x to b.', ws);
    expect(newws.a).toEqual(4)
    expect(newws.b).toEqual(6)
});

it('perform procedure 10 times passing argument', () => {
    var ws = { k: null, a: 0 };
    var newws = run('perform procedure1 using k varying k from 1 to 4. procedure1 section using x. add x to a.', ws);
    expect(newws.a).toEqual(10)
    expect(newws.k).toEqual(5)
});

it('perform with giving and return', () => {
    var ws = { result: 0 };
    var newws = run('perform procedure1 using 3 giving result. procedure1 section using n. add 1 to n. return n.', ws);
    expect(newws.result).toEqual(4)
});

it('perform factorial with auxiliary parameters', () => {
    var ws = { result: 0 };
    var newws = run('perform factorial using 3 giving result. factorial section using n. local m. if n = 1 then return n. subtract 1 from n giving m. perform factorial using m giving m. multiply n by m. return m.', ws);
    expect(newws.result).toEqual(6)
});

it('perform global function', () => {
    var result = 1;
    global.foo = function() { result = 2; };

    var newws = run('global foo. perform foo.');
    expect(result).toEqual(2)
});

it('invoke require', () => {
    global.foo = null;
    global.require = require;
    var assert = require('assert');

    var newws = run('global foo. global require. perform require using "assert" giving foo.');
    expect(global.foo).toEqual(assert)
});

it('perform function in global object', () => {
    var result = 1;
    global.foo = {
        setresult: function(n) { result = n; }
    };

    var newws = run('global foo. perform setresult in foo using 2.');
    expect(result).toEqual(2)
});

it('perform Math cosine', () => {
    var ws = { result: null };
    var newws = run('perform cos in Math using 10 giving result.', ws);
    expect(!!newws.result).toBe(true);
    expect(newws.result).toEqual(Math.cos(10))
});

it('perform inline perform', () => {
    var ws = { result: null };
    var newws = run('local k. move 0 to result. perform varying k from 1 to 4 add k to result end-perform.', ws);
    expect(!!newws.result).toBe(true);
    expect(newws.result).toEqual(10)
});

it('perform inline perform with exit perform', () => {
    var ws = { result: null };
    var newws = run('local k. move 0 to result. perform varying k from 1 to 4 add k to result if k >= 3 then exit perform end-perform.', ws);
    expect(!!newws.result).toBe(true);
    expect(newws.result).toEqual(6)
});

it('perform inline perform setting array values', () => {
    var ws = { a: [1, 2, 3, 4] };
    var newws = run('local k. perform varying k from 1 to 4 add 1 to a(k) end-perform.', ws);
    expect(!!newws.a).toBe(true);
    expect(newws.a.toString()).toEqual('2,3,4,5')

    var ws = { a: [0, 0, 0, 0], b: [0, 0, 0, 0] };
    var newws = run('local k. perform varying k from 1 to 4 move k to a(k) b(k) end-perform.', ws);
    expect(!!newws.a).toBe(true);
    expect(newws.a.toString()).toEqual('1,2,3,4')
    expect(!!newws.b).toBe(true);
    expect(newws.b.toString()).toEqual('1,2,3,4')
});

