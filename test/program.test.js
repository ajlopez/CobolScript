
var cobs = require('../lib/cobolscript'),
    path = require('path');
    
it('compile program defined', () => {
    expect(!!cobs.compileProgram).toBe(true);
});

it('data division', () => {
    var program = cobs.compileProgram('data division.procedure division.return.');
    expect(!!program).toBe(true);
    expect(typeof program.data).not.toEqual( 'undefined');
});

it('working-storage section', () => {
    var program = cobs.compileProgram('data division. working-storage section. procedure division.return.');
    expect(!!program).toBe(true);
    expect(!!program.data).toBe(true);
    expect(typeof program.data.working_storage).not.toEqual( 'undefined');
});

it('linkage section', () => {
    var program = cobs.compileProgram('data division. linkage section. procedure division.return.');
    expect(!!program).toBe(true);
    expect(!!program.data).toBe(true);
    expect(typeof program.data.linkage).not.toEqual( 'undefined');
});

it('linkage section with item', () => {
    var program = cobs.compileProgram('data division. linkage section. 01 request. 01 response. procedure division.return.');
    expect(!!program).toBe(true);
    expect(!!program.data).toBe(true);
    expect(!!program.data.linkage).toBe(true);
    expect(typeof program.data.linkage.request).not.toEqual( 'undefined');
    expect(typeof program.data.linkage.response).not.toEqual( 'undefined');
});

